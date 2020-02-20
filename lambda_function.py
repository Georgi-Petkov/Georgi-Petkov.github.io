
import pandas as pd
import httplib2
from apiclient.discovery import build
from oauth2client.service_account import ServiceAccountCredentials


def get_report():
  credentials = ServiceAccountCredentials.from_json_keyfile_name(
                     '/home/rstudio/memorix-bc834655fa79.json',
        'https://www.googleapis.com/auth/analytics.readonly',)
  http = credentials.authorize(httplib2.Http())
    # The URL of the discovery service.
  DISCOVERY_URI = (
        'https://analyticsreporting.googleapis.com/$discovery/rest')
    # Now we build the API connection. In that case we want to call 
    # the 'analytics' API version 4.
  analytics = build('analytics', 'v4', http=http, discoveryServiceUrl=DISCOVERY_URI)

  VIEW_ID = '201387453'
  DIMENSIONS = ['ga:source','ga:medium','ga:dimension2']
  METRICS = ['ga:users','ga:sessions']
  return analytics.reports().batchGet(
      body={
        'reportRequests': [
        {
          'viewId': VIEW_ID,
          #'dateRanges': [{'startDate': '7daysAgo', 'endDate': 'today'}],
          'dateRanges': [{'startDate': '28daysAgo', 'endDate': 'today'}],
          'metrics': [{'expression':i} for i in METRICS],
          'includeEmptyRows': True,
          'pageSize': 10000,
          'pageToken': 'abc',
          'dimensions': [{'name':j} for j in DIMENSIONS]
        }]
      }
  ).execute()


def convert_to_dataframe(response):
    
  for report in response.get('reports', []):
    columnHeader = report.get('columnHeader', {})
    dimensionHeaders = columnHeader.get('dimensions', [])
    metricHeaders = [i.get('name',{}) for i in columnHeader.get('metricHeader', {}).get('metricHeaderEntries', [])]
    finalRows = []
    

    for row in report.get('data', {}).get('rows', []):
      dimensions = row.get('dimensions', [])
      metrics = row.get('metrics', [])[0].get('values', {})
      rowObject = {}

      for header, dimension in zip(dimensionHeaders, dimensions):
        rowObject[header] = dimension
        
        
      for metricHeader, metric in zip(metricHeaders, metrics):
        rowObject[metricHeader] = metric

      finalRows.append(rowObject)
      
      
  dataFrameFormat = pd.DataFrame(finalRows)    
  return dataFrameFormat      

def main():
    analytics = initialize_analyticsreporting()
    response = get_report()
    ga_df = convert_to_dataframe(response)   #df = pandas dataframe
    #export_to_sheets(df)                  #outputs to spreadsheet. comment to skip
    print(ga_df.to_json(orient='records'))
    return(ga_df.to_json(orient='records'))
  
  #ga_df.to_csv('export_ga_df',index=False)
 
 
# 
# if __name__ == '__main__':
#   main()
def lambda_handler(event, content):
    """ entity point into lambda function """
    ga_df = main()
    return(ga_df)
