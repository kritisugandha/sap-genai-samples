-- Create Model for Generating Multi-modal embeddings
CREATE OR REPLACE MODEL `your-project-id.epm_shop.mm_embedding`
  REMOTE WITH CONNECTION `us.vertex_ai`
  OPTIONS(ENDPOINT = 'multimodalembedding@001');

-- Create Table for Storing Product Info from SAP
CREATE OR REPLACE TABLE `your-project-id.epm_shop.product_info_sap` (
entity_id STRING NOT NULL, 
last_updated TIMESTAMP NOT NULL,  
product_id STRING NOT NULL,
product_rating INTEGER NOT NULL,
stock_quantity INTEGER NOT NULL,
quantity_unit STRING NOT NULL,
product_name STRING NOT NULL,
imageid STRING NOT NULL,
feature_timestamp TIMESTAMP NOT NULL 
)
PARTITION BY DATE(last_updated); 

-- Create Table for Storing Product Reviews from SAP
CREATE TABLE `your-project-id.epm_shop.product_reviews_sap`
(
  entity_id STRING NOT NULL,
  last_updated TIMESTAMP NOT NULL,
  product_id STRING NOT NULL,
  review_text STRING NOT NULL,
  product_rating INT64 NOT NULL,
  feature_timestamp TIMESTAMP NOT NULL
)
PARTITION BY DATE(last_updated);

-- Create External Table for Product Images stored in GCS bucket
CREATE OR REPLACE EXTERNAL TABLE `your-project-id.epm_shop.image_object_table`
WITH CONNECTION `your-project-id.us.vertex_ai`
OPTIONS(
   object_metadata = 'SIMPLE',
   uris = [
           'gs://your-gcs-bucket-name/*'
          ]
);

-- Create and populate Table for Storing Embeddings
CREATE OR REPLACE TABLE `your-project-id.epm_shop.merch_store_embeddings` AS (
SELECT *,
REGEXP_EXTRACT (uri, r'\/([^\/]+)$') AS obj_name,
REGEXP_REPLACE(REGEXP_EXTRACT (uri, r'\/([^\/]+)$'), r'\.jpg$', '') AS product_id,
 '' AS product_name
FROM ML.GENERATE_EMBEDDING (
MODEL`your-project-id.epm_shop.mm_embedding`, 
TABLE `your-project-id.epm_shop.image_object_table`
 )
);

-- Sample Query for Vector Search
SELECT base.product_name, base.uri, base.product_id, base.obj_name, distance
FROM VECTOR_SEARCH(
TABLE `your-project-id.epm_shop.merch_store_embeddings`, 'ml_generate_embedding_result',
(
SELECT ml_generate_embedding_result AS embedding_col
FROM ML.GENERATE_EMBEDDING
(
MODEL `your-project-id.epm_shop.mm_embedding`,
(SELECT "In stock Rainbow colored items with bad reviews" AS content),
  STRUCT(TRUE AS flatten_json_output)

)
)
, top_k => 5
);