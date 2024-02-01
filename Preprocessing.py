import pandas as pd
import numpy as np



hosted_country = ["USA", "NED", "BEL", "GRE", "USA", "ESP", "CHN",
                  "GER", "CAN", "FRA", "ITA", "GER", "FRA", "FIN",
                  "AUT", "USA", "NOR", "GBR", "USA", "AUS", "MEX", "CAN",
                  "RUS", "GER", "JPN", "NOR", "FRA", "BRA", "ITA",
                  "USA", "SUI", "JPN", "BIH", "KOR", "RUS", "USA",
                  "USA", "SWE", "AUS", "JPN", "ITA", "CAN"]

olympic = pd.read_csv("olympic_cleaned.csv")

olympic

Sport = np.unique(olympic["Sport"])
Sport

Type = ['Individual', 'Individual', 'Individual', 'Individual',
                      'Individual', 'Team', 'Team', 'Team',
                      'Individual', 'Team', 'Individual', 'Individual',
                      'Individual', 'Team', 'Individual', 'Individual',
                      'Individual', 'Individual', 'Individual', 'Team',
                      'Individual', 'Individual', 'Both', 'Team', 'Team',
                      'Team', 'Individual', 'Team', 'Individual', 'Individual',
                      'Both', 'Individual', 'Team', 'Individual', 'Team',
                      'Team', 'Individual', 'Individual', 'Individual', 'Individual',
                      'Individual', 'Team', 'Individual', 'Individual', 'Team',
                      'Individual', 'Individual', 'Individual', 'Individual', 'Individual',
                      'Individual', 'Individual', 'Both', 'Team', 'Individual',
                      'Individual', 'Both', 'Team', 'Individual', 'Individual']

Sport_df = olympic["Sport"].to_numpy()

Sport_type = []
for sport in Sport_df:
  ind = np.where(Sport == sport)[0][0]
  Sport_type.append(Type[ind])

olympic["Sport_type"] = Sport_type

np.unique(olympic["Medal"])

# Quantify medal by the 2008 controversy over medal rank,
# Jeff Z. Klein in a New York Times blog post proposed a 4:2:1 system as a
# compromise between the total-medals and golds-first methods.
Medal_Score = []
Medal = olympic["Medal"].to_numpy()
for medal in Medal:
  if medal == 'Bronze':
    Medal_Score.append(1)
  elif medal == "Gold":
    Medal_Score.append(4)
  elif medal == "No Medal":
    Medal_Score.append(0)
  elif medal == "Silver":
    Medal_Score.append(2)

olympic["Medal_Score"] = Medal_Score

olympic = pd.get_dummies(olympic, columns = ["Sex", "Sport_type"])



olympic_impact = olympic.pivot_table(['Comes_from_City'], ['Year', 'NOC'], aggfunc='sum').reset_index()

df2 = olympic.pivot_table(['Age'], ['Year', 'NOC'], aggfunc='mean').reset_index()

df3 = olympic.pivot_table(['Height'], ['Year', 'NOC'], aggfunc='mean').reset_index()
df4 = olympic.pivot_table(['Weight'], ['Year', 'NOC'], aggfunc='mean').reset_index()

df5 = olympic.pivot_table(['Medal_Score'], ['Year', 'NOC'], aggfunc='sum').reset_index()

df6 = olympic.pivot_table(['ID'], ['Year', 'NOC'], aggfunc='count').reset_index()

df7 = olympic.pivot_table(['Sport_type_Both'], ['Year', 'NOC'], aggfunc='sum').reset_index()

df8 = olympic.pivot_table(['Sport_type_Individual'], ['Year', 'NOC'], aggfunc='sum').reset_index()

df9 = olympic.pivot_table(['Sport_type_Team'], ['Year', 'NOC'], aggfunc='sum').reset_index()

df10 = olympic.pivot_table(['Sex_F'], ['Year', 'NOC'], aggfunc='sum').reset_index()

df11 = olympic.pivot_table(['Sex_M'], ['Year', 'NOC'], aggfunc='sum').reset_index()

olympic_impact["Avg_Age"] = df2["Age"]

olympic_impact["Avg_Height"] = df3["Height"]
olympic_impact["Avg_Weight"] = df4["Weight"]
olympic_impact["Total_Medal_Score"] = df5["Medal_Score"]

olympic_impact["Number_of_Athelete"] = df6["ID"]
olympic_impact["Sport_type_Both"] = df7["Sport_type_Both"]
olympic_impact["Sport_type_Individual"] = df8["Sport_type_Individual"]
olympic_impact["Sport_type_Team"] = df9["Sport_type_Team"]
olympic_impact["Sex_F"] = df10["Sex_F"]
olympic_impact["Sex_M"] = df11["Sex_M"]

olympic_impact



olympic_impact.to_csv('olympic_impact.csv', index = False)



