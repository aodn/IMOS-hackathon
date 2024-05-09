
# MLD-chooser.py

# 08/05/2024
# Natalia Ribeiro (IMOS) and Michael Hemming (NSW-IMOS)

# %% ----------------------------------------------------------------------------
# Import packages

import os
os.chdir('C:\\Users\\mphem\\OneDrive - UNSW\\Work\\QAQC_NRT_AODNhackathon_2024\\' + 
         'AODNhackathon\\aodn-hackathon\\2024\\Projects\\MLD-GUI')

import tkinter as tk
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import PickleStuff as ps # functions to save/load pickle files
import numpy as np
import pandas as pd
from datetime import datetime

# %% ----------------------------------------------------------------------------
# Function to get season from datetime64

def get_season(date):
    month = date.astype('datetime64[M]').astype(int) % 12 + 1
    if month in [3, 4, 5]:
        return 'Autumn'
    elif month in [6, 7, 8]:
        return 'Winter'
    elif month in [9, 10, 11]:
        return 'Spring'
    else:
        return 'Summer'

# %% ----------------------------------------------------------------------------
# Load data and setup for GUI

PSAL = ps.PickleLoad('Data\PH100CTD_PSAL.pickle')
TEMP = ps.PickleLoad('Data\PH100CTD_TEMP.pickle')
DENS = ps.PickleLoad('Data\PH100CTD_DENS.pickle')

# example profiles for testing
nprofs = 3
un_t = np.unique(PSAL['PH100']['PSAL'].TIME.values)
random_selection = np.random.choice(un_t, size=nprofs)

temperature_profiles = []
salinity_profiles = []
density_profiles = []
dates = []
times = []
seasons = []

for n in range(len(random_selection)):
    c = PSAL['PH100']['PSAL'].TIME.values == random_selection[n]
    
    # Combine arrays into tuples
    TEMP_tuple = (TEMP['PH100']['TEMP'].DEPTH.values[c], TEMP['PH100']['TEMP'].values[c])
    PSAL_tuple = (PSAL['PH100']['PSAL'].DEPTH.values[c], PSAL['PH100']['PSAL'].values[c])
    DENS_tuple = (DENS['PH100']['DENS'].DEPTH.values[c], DENS['PH100']['DENS'].values[c])

    
    temperature_profiles.append(TEMP_tuple)
    salinity_profiles.append(PSAL_tuple)
    density_profiles.append(DENS_tuple)
    
    # get other information
    # Given numpy.datetime64 object
    datetime_obj = random_selection[n]
    
    # Convert to Python datetime object
    python_datetime_obj = datetime.utcfromtimestamp(datetime_obj.astype('O') / 1e9)
    
    # Extract date and time components
    date_component = python_datetime_obj.date()
    time_component = python_datetime_obj.time()

    dates.append(str(date_component))
    times.append(str(time_component)[0:5])
    seasons.append(get_season(random_selection[n]))

site = 'PH100'

# %% ----------------------------------------------------------------------------

class OceanProfileGUI:
    def __init__(self, root, temperature_profiles, salinity_profiles, density_profiles, dates, times, seasons, site):
        self.root = root
        self.root.title("Ocean Profile Analysis")

        self.site = site
        self.dates = dates
        self.times = times
        self.seasons = seasons
        self.temperature_profiles = temperature_profiles
        self.salinity_profiles = salinity_profiles
        self.density_profiles = density_profiles
        self.num_profiles = len(temperature_profiles)
        self.current_profile_index = 0  # Index of the currently displayed profile

        # Create a figure with one subplot
        self.fig, self.plot_area = plt.subplots(figsize=(8, 6))

        self.update_profiles()  # Initial plot update

        self.canvas_widget = FigureCanvasTkAgg(self.fig, master=root)
        self.canvas_widget.get_tk_widget().pack(side=tk.LEFT, fill=tk.BOTH, expand=True)  # Fill window and expand to fit

        self.selected_depth = None
        self.recorded_depths = []  # List to store recorded depths

        # Create a side box to display profile index and depth
        self.side_box = tk.Text(root, height=10, width=20)
        self.side_box.pack(side=tk.RIGHT)

        # Connect event handlers
        self.canvas_widget.mpl_connect('motion_notify_event', self.on_hover_profile)
        self.canvas_widget.mpl_connect('button_press_event', self.on_click_profile)

        self.update_side_box()  # Update the side box with current profile index

        self.next_button = tk.Button(root, text="Next", command=self.load_next_profile)
        self.next_button.pack()

        # Store the reference to the red dashed line
        self.red_lines = []

    def update_profiles(self):
        self.plot_area.clear()

        # Plot temperature data
        depth, temperature = self.temperature_profiles[self.current_profile_index]
        self.plot_area.plot(temperature, depth, label='Temperature (°C)')
        self.plot_area.set_xlabel("Temperature (°C)")
        self.plot_area.set_ylabel("Depth (m)")
        
        # Invert the y-axis
        self.plot_area.invert_yaxis()
        
        # Add annotation
        annotation_text = (self.site + ' ' + self.dates[self.current_profile_index] + ' ' 
                           + self.times[self.current_profile_index] + ' (' 
                           + self.seasons[self.current_profile_index] + ')')# Annotation text
        annotation_x = 0.5  # X-coordinate of annotation
        annotation_y = -0.13  # Y-coordinate of annotation 
        
        # Annotate the plot
        self.plot_area.text(annotation_x, annotation_y, annotation_text,
                            horizontalalignment='center',
                            verticalalignment='bottom',
                            fontsize=12,
                            transform=self.plot_area.transAxes)
        

        # Plot salinity data on secondary y-axis
        depth, salinity = self.salinity_profiles[self.current_profile_index]
        salinity_axis = self.plot_area.twiny()
        salinity_axis.plot(salinity, depth, label='Salinity', color='orange')
        salinity_axis.set_xlabel("Salinity")

        # Plot density data on secondary y-axis, slightly higher than salinity axis
        depth, density = self.density_profiles[self.current_profile_index]
        density_axis = self.plot_area.twiny()
        density_axis.plot(density, depth, label='Density', color='green')
        density_axis.set_xlabel("Density [kg m-3]")
        density_axis.spines['top'].set_position(('outward', 30))  # Adjust the position of the density axis

        # Combine the legends
        self.plot_area.legend(loc='upper right')

        self.plot_area.grid(True)  # Add grid lines

    def load_next_profile(self):
        # Record the last clicked depth if available before moving to the next profile
        if self.selected_depth is not None:
            self.recorded_depths.append(self.selected_depth)

        # Load and plot the next profile if not all profiles have been displayed
        if self.current_profile_index < self.num_profiles - 1:
            self.current_profile_index += 1
            self.update_profiles()  # Update plot with new data
            self.update_side_box()  # Update the side box with current profile index
            self.remove_red_lines()  # Remove previous red lines
            self.canvas_widget.draw()
        else:
            # Close the GUI if all profiles have been displayed
            self.root.quit()
            self.root.destroy()

    def on_hover_profile(self, event):
        # Display depth in side box as you hover over the profile
        if event.xdata is not None and event.ydata is not None:
            depth = event.ydata
            self.update_side_box(hover_depth=depth)  # Update the side box with current profile index and hover depth

            # Draw red horizontal dashed line following cursor
            if self.red_lines:
                for red_line in self.red_lines:
                    red_line.set_ydata(depth)
            else:
                red_line = self.plot_area.axhline(depth, color='r', linestyle='--')
                self.red_lines.append(red_line)

            self.canvas_widget.draw()

    def on_click_profile(self, event):
        # Record depth when clicking on profile
        if event.xdata is not None and event.ydata is not None:
            self.selected_depth = event.ydata
            self.update_side_box()  # Update the side box with current profile index and MLD

    def update_side_box(self, hover_depth=None):
        # Update the side box with current profile index and depth
        self.side_box.delete(1.0, tk.END)  # Clear previous content
        self.side_box.insert(tk.END, f"Profile: {self.current_profile_index + 1}/{self.num_profiles}\n")
        if self.selected_depth is not None:
            self.side_box.insert(tk.END, f"MLD Depth: {self.selected_depth:.2f} m\n")
        if hover_depth is not None:
            self.side_box.insert(tk.END, f"Depth: {hover_depth:.2f} m\n")

    def remove_red_lines(self):
        for red_line in self.red_lines:
            red_line.remove()
        self.red_lines.clear()


if __name__ == "__main__":
    # Assuming temperature_profiles, salinity_profiles, and density_profiles are the variables containing the loaded profiles

    # Create the GUI
    root = tk.Tk()
    gui = OceanProfileGUI(root, temperature_profiles, salinity_profiles, density_profiles, dates, times, seasons, site)
    root.mainloop()

    # Output recorded depths
    print("MLD Recorded Depths:", gui.recorded_depths)

# %% ----------------------------------------------------------------------------
# create dataframe and save output as a CSV

df = pd.DataFrame({'Profile n': np.arange(1,nprofs+1,1), 
                   'Date': np.array(dates),
                   'Time (UTC)': np.array(times),
                   'Austral Season': np.array(seasons),
                   'MLD recorded [m]': np.round(gui.recorded_depths,0)})
# set index as profile n
df.set_index('Profile n', inplace=True)

# Get the current time
current_time = datetime.now()
# Format the current time
formatted_time = current_time.strftime('%Y-%m-%d_%H-%M')

df.to_csv('Data/MLD-recorded_' + formatted_time + '.csv')





