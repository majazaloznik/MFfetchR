INSERT INTO test_platform.source(
  id, name, name_long, url)
VALUES (2, 'MF', 'Ministrstvo za Finance RS', 'https://www.gov.si/teme/fiskalna-in-javnofinancna-politika/')
on conflict(id) do update
set name = 'MF',
name_long = 'Ministrstvo za finance RS',
url = 'https://www.gov.si/teme/fiskalna-in-javnofinancna-politika/';
