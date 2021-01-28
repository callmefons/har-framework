# Firebase Cloud Messaging for BL

import pymysql
import requests
import sys
import pandas as pd

api_key = str(sys.argv[5]) 
api_url = 'https://fcm.googleapis.com/fcm/send'
headers = {'Content-Type': 'application/json',
           'Authorization': 'key='+api_key}

from datetime import date, datetime

def json_serial(obj):
    if isinstance(obj, (datetime, date)):
        return obj.isoformat()
    raise TypeError ("Type %s not serializable" % type(obj))
    
    
def find_working_time(diff):
    days, seconds = diff.days, diff.seconds
    hours = days * 24 + seconds // 3600
    minutes = (seconds % 3600) // 60
    seconds = seconds % 60
    
    return hours,minutes,seconds

# connect database server
DB_HOSTNAME = str(sys.argv[1]) 
DB_USERNAME = str(sys.argv[2]) 
DB_PASSWORD = str(sys.argv[3]) 
DB_NAME = str(sys.argv[4]) 

db = pymysql.connect(host=DB_HOSTNAME, port=3306, user=DB_USERNAME, passwd=DB_PASSWORD, db=DB_NAME)

cursor = db.cursor()

cursor.execute("SELECT id FROM users WHERE state = 'active';")
users = cursor.fetchall()
df = []
df_cheating = []
cheating_id = 999

for user in users:
    temp = ""
    cursor.execute("SELECT * from (SELECT users.id, users.firebase_token, users.created_at, ROUND(accuracies.value,2), accuracies.activity_type_id, activity_types.name, accuracies.accuracy_type, accuracies.updated_at "+ 
    "FROM users INNER JOIN accuracies ON users.id = accuracies.user_id INNER JOIN activity_types ON activity_types.id = accuracies.activity_type_id "+
    "WHERE accuracies.activity_type_id != (%s) AND accuracies.user_id = (%s) AND accuracies.accuracy_type = 'baseline') AS total " + 
    "WHERE total.updated_at = (SELECT MAX(updated_at) FROM accuracies WHERE activity_type_id != (%s) AND user_id = (%s)) ", (cheating_id,user,cheating_id,user))
    results = cursor.fetchall()
    
    cursor.execute("SELECT COUNT(*) from activities where user_id = (%s)", user)
    uploaded_labels = cursor.fetchall()
    
    cursor.execute("SELECT * from (SELECT users.id, users.firebase_token, users.created_at, ROUND(accuracies.value,2), accuracies.activity_type_id, activity_types.name, accuracies.accuracy_type, accuracies.updated_at "+ 
    "FROM users INNER JOIN accuracies ON users.id = accuracies.user_id INNER JOIN activity_types ON activity_types.id = accuracies.activity_type_id "+
    "WHERE accuracies.activity_type_id = (%s) AND accuracies.user_id = (%s)) AS total " + 
    "WHERE total.updated_at = (SELECT MAX(updated_at) FROM accuracies WHERE activity_type_id = (%s) AND user_id = (%s)) ", (cheating_id,user,cheating_id,user))
    cheating = cursor.fetchall()
    print(len(cheating))

    if(len(cheating) > 0):
        df_cheating = pd.DataFrame(list(cheating), columns=["user_id", "firebase_token", "user_created_at","value", "activity_type_id", "activity_type", "accuracy_type", "updated_at"])
        cheating_rate = round(df_cheating["value"][0],2)
    
    if len(results) > 0 and uploaded_labels[0][0] > 0:
        df = pd.DataFrame(list(results), columns=["user_id", "firebase_token", "user_created_at","value", "activity_type_id", "activity_type", "accuracy_type", "updated_at"])
        df["uploaded_labels"] = uploaded_labels[0][0]
        
        token = df["firebase_token"][0]
        
        uploaded_labels = "%s labels" % (df["uploaded_labels"][0])
        user_created_at = json_serial(df.user_created_at[0])

        now = datetime.now()            
        diff = now - df.user_created_at[0]

        hours, minutes, seconds = find_working_time(diff)

        woking_time = "%s hr(s) %s min(s)" % (hours, minutes)
        
        data = {
                "to": token,
                "data": {
                    "type": "score",
                    "method": "baseline",
                    "uploaded_labels": uploaded_labels,
                    "scores": str(df.value[0]),
                    "start_time": user_created_at,
                    "working_time": woking_time,
                    "cheating_rate": cheating_rate
                    }
        }
        print(data)
        response = requests.post(api_url, headers=headers, json=data)
        print(response.text)