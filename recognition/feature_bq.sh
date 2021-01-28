#!/bin/sh

#cd /home/ubuntu/eneact/
bqpath=/home/ubuntu/google-cloud-sdk/bin/bq
gsutil=/home/ubuntu/google-cloud-sdk/bin/gsutil

app=$@
echo $app

sql="SELECT
  minute AS time,
  user_id AS user,
  AVG(x) AS mean_x,
  MAX(x) AS max_x,
  MIN(x) AS min_x,
  STDDEV(x) AS sd_x,
  AVG(y) AS mean_y,
  MAX(y) AS max_y,
  MIN(y) AS min_y,
  STDDEV(y) AS sd_y,
  AVG(z) AS mean_z,
  MAX(z) AS max_z,
  MIN(z) AS min_z,
  STDDEV(z) AS sd_z
FROM (
  SELECT
    user_id,
    datetime,
    CAST(x AS float64) AS x,
    CAST(y AS float64) AS y,
    CAST(z AS float64) AS z,
    TIMESTAMP_TRUNC( SAFE.PARSE_TIMESTAMP('%Y-%m-%dT%H:%M:%E3S%z', REGEXP_REPLACE(datetime, ':00$', '00')), MINUTE) AS minute
  FROM (
    SELECT
      user_id,
      datetime,
      x,
      y,
      z
    FROM
      ${app}.accs
    GROUP BY
      user_id,
      datetime,
      x,
      y,
      z))
WHERE minute > TIMESTAMP_SUB(CURRENT_TIMESTAMP, INTERVAL 2 DAY)
GROUP BY
  user_id,
  minute;"

echo "$sql"   | $bqpath query --destination_table=${app}.tmp_features --replace=true --use_legacy_sql=false --format=csv
$bqpath wait
$bqpath extract ${app}.tmp_features gs://eneact/tmp_features*.csv
$gsutil cp gs://eneact/tmp_features*.csv ./db/rdata/${app}/

$gsutil rm gs://eneact/tmp_features*.csv
