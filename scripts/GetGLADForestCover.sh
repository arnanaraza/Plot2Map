#!/bin/bash

# Base URL for the TCC_2010 files
base_url="https://glad.umd.edu/Potapov/TCC_2010/"

# Directory to save the files in (create the directory if it doesn't exist)
save_dir="$HOME/Documents/GFC/data/TCC_2010"
mkdir -p "$save_dir"

# Loop through both North and South latitudes
for lat_direction in N S; do
    for lat in $(seq -f "%02g" 0 10 80); do  # Use seq for latitude values
        for lon in $(seq -f "%03g" 0 10 170); do  # Use seq for longitude values
            # Construct the filename for the latitude
            filename="treecover2010_${lat}${lat_direction}_${lon}E.tif"
            file_path="${save_dir}/${filename}"
            
            # Check if the file already exists
            if [ ! -f "$file_path" ]; then
                # Download the file for the Eastern longitudes if not exists
                wget -P "$save_dir" "${base_url}${filename}"
            else
                echo "$filename already exists, skipping download."
            fi
            
            # Handle files with 'W' suffix for western longitudes
            if [[ "$lon" -ge 0 && "$lon" -lt 100 ]]; then
                lon_w=$((lon + 10))
                filename_w="treecover2010_${lat}${lat_direction}_$(printf "%03d" $lon_w)W.tif"
                file_path_w="${save_dir}/${filename_w}"
                
                # Check if the file already exists
                if [ ! -f "$file_path_w" ]; then
                    # Download the file for the Western longitudes if not exists
                    wget -P "$save_dir" "${base_url}${filename_w}"
                else
                    echo "$filename_w already exists, skipping download."
                fi
            fi
        done
    done
done

echo "Download complete."
