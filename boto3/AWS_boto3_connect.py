import boto3
import os
from botocore.exceptions import ClientError

os.getcwd()


AWS_KEY_ID = 'XXXXXXXXXXXXXXXXXXXXXXX'
AWS_SECRET ='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'


s3 = boto3.resource('s3')



rds = boto3.client('rds',region_name='eu-west-1',
                  aws_access_key_id=AWS_KEY_ID,
                  aws_secret_access_key=AWS_SECRET)
                  
  rds.describe_db_instances()




rds.describe_db_security_groups()
                  
s3 = boto3.client('s3',region_name='eu-west-1',
                  aws_access_key_id=AWS_KEY_ID,
                  aws_secret_access_key=AWS_SECRET)

# specify the region of the bucket-------------------
#response_staging = s3.create_bucket(Bucket='gim-staging1',CreateBucketConfiguration={
#        'LocationConstraint': 'eu-north-1'})
        
# List the buckets
response = s3.list_buckets()

response['Buckets']
# Iterate over Buckets from .list_buckets() response
for bucket in response['Buckets']:
  
  	# Print the Name for each bucket
    print(bucket['Name'])

# delete bucket-------------------
s3.delete_bucket(Bucket='gim-staging1')

#######

# Upload the file.csv to name of bucket
s3.upload_file(
  # Complete the filename
  Filename='./AWS_RDS_new_worlds.py', 
  # Set the key and bucket
  Key='lambda.py', 
  Bucket='memorixpythoncode',
  # During upload, set ACL to public-read
  ExtraArgs = {
    'ACL': 'public-read'})

# ec2 = boto3.client('ec2',region_name='eu-north-1',
#                   aws_access_key_id=AWS_KEY_ID,
#                   aws_secret_access_key=AWS_SECRET)
                  
                  
ec2 = boto3.resource('ec2', region_name='eu-west-1',
                  aws_access_key_id=AWS_KEY_ID,
                  aws_secret_access_key=AWS_SECRET)

instances =ec2.instances.filter(Filters=[{'Name': 'instance-state-name', 'Values': ['*']}])

instances = ec2.instances.filter(
    Filters=[{'Name': 'instance-state-name', 'Values': ['running']}])
    
for instance in instances:
    print(instance.id, instance.instance_type, instance.key_name, instance.private_ip_address, instance.public_dns_name ,instance.tags[0].get("Value"))

# chose instance by selecting instance id------------------------------
instance = ec2.Instance('i-00ba22d35189c5afc')

# get state of the instance------------------------------
instance.state

# stop instances---------------------------
instance.stop()

   
   ec2.stop_instances(InstanceIds=ids)
   ec2.instances()
# List only objects that start with '2019/final_'
response = s3.list_objects(
    Bucket='memorixpythoncode', Prefix='*')

# Iterate over the objects
for obj in response['Contents']:
  
    # Give each object ACL of public-read
    s3.put_object_acl(Bucket='gid-staging', 
                      Key=obj['Key'], 
                      ACL='public-read')
    
    # Print the Public Object URL for each object
    print("https://{}.s3.amazonaws.com/{}".format('gid-staging', obj['key']))
