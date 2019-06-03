cd %CD%
jupyter nbconvert --to notebook --execute Cab_fare.ipynb --allow-errors --ExecutePreprocessor.timeout=180 mynotebook.ipynb 