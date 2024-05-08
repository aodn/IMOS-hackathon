# Loading libraries
import json

import pandas as pd
import requests
import xarray as xr


# Defining functions
# Extracting bounding box from GPE3 necdf files
def bb_from_gpe3(filename: str, tag_id: str) -> pd.DataFrame:
    '''
    Inputs:
      - filename - (string) Location of the GPE3 netcdf file
      - tag_id - (integer) Unique tag ID
    Outputs:
      - bb_df - (pandas DataFrame) Bounding box and tag ID
    '''

    # Open dataset
    ds = xr.open_dataset(filename)

    # Store in dictionary
    bb_dict = {
        'PTT': tag_id,  # Extracting bounding box (max and min coordinates)
        'min_lat': ds.coords['latitude'].min().values,
        'max_lat': ds.coords['latitude'].max().values,
        'min_lon': ds.coords['longitude'].min().values,
        'max_lon': ds.coords['longitude'].max().values,
    }

    # Turn to data frame
    bb_df = pd.DataFrame([bb_dict])

    # Return data frame
    return bb_df


def get_fishing_effort(
    token: str, date_start: str, date_end: str, bbox_df: pd.DataFrame
) -> pd.DataFrame:
    """
    Get fishing effort data from 4wings report endpoint.
    Returns a pandas dataframe with fishing_hours, lat, lon columns,
    and one row for each cell.
    """

    bbox = bbox_df["min_lon", "min_lat", "max_lon", "max_lat"].values[0]

    endpoint = f"https://gateway.api.globalfishingwatch.org/v3/4wings/report"
    auth_header = {"Authorization": f"Bearer {token}"}
    query_params = {
        "spatial-resolution": "HIGH",
        "temporal-resolution": "ENTIRE",
        "date-range": f"{date_start},{date_end}",
        "datasets[0]": "public-global-fishing-effort:latest",
        "format": "JSON",
    }
    data = json.dumps(
        {
            "geojson": {
                "type": "Polygon",
                "coordinates": [
                    [
                        [bbox[0], bbox[1]],
                        [bbox[2], bbox[1]],
                        [bbox[2], bbox[3]],
                        [bbox[0], bbox[3]],
                        [bbox[0], bbox[1]],
                    ]
                ],
            }
        }
    )
    result = requests.post(endpoint, data, params=query_params, headers=auth_header)
    if result.status_code != 200:
        print(f"Error: {result.status_code}")
        print(result.content)
        return
    data = json.loads(result.content)
    assert len(data['entries']) == 1, "Only one entry should be returned"
    entries = data['entries'][0]
    assert len(entries) == 1, "Only one dataset should be returned"
    for value in entries.values():
        table = pd.DataFrame.from_dict(value)
    table = table.drop(columns=["date"])
    table = table.rename(columns={"hours": "fishing_hours"})
    return table


def download_gfw_data(
    filename: str, tag_id: str, token: str, date_start: str, date_end: str
) -> pd.DataFrame:
    """
    Download GFW fishing effort data for a given tag ID and date range,
    extracting the bounding box from the GPE3 netcdf file.
    Returns a pandas dataframe with fishing_hours, lat, lon columns,
    and one row for each cell.
    """

    bbox_df = bb_from_gpe3(filename, tag_id)
    effort_df = get_fishing_effort(token, date_start, date_end, bbox_df)

    return effort_df
