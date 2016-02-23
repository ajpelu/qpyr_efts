### Get raw data

A query to obtain raw data by pixel (only pixels covering *Q. pyreanica* populations in Sierra Nevada)

```sql 
SELECT 
  iv.iv_malla_modi_id, 
  iv.fecha, 
  iv.evi, 
  iv.ndvi, 
  iv.composite_day_year,
  aux.lng, 
  aux.lat, 
  aux.poblacion 
FROM 
  public.aux_ms_ontologias_iv_malla_modis_completa as aux, 
  public.iv_modis as iv
WHERE 
  aux.id = iv.iv_malla_modi_id;
```

Ouput result is saved as: `/data/raw_iv.csv`  


