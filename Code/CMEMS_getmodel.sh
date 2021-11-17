python -m motuclient --motu https://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -70 --longitude-max -25 --latitude-min -70 --latitude-max -45 --date-min "1993-01-01 12:00:00" --date-max "2019-12-31 12:00:00" --depth-min 0.494 --depth-max 5727.917 --variable bottomT --variable mlotst --variable so --variable thetao --variable uo --variable vo --out-dir CMEM_030_Nov --out-name CMEM_030_WholeModel --user oscott --pwd CMEMS_SCOTT_2021