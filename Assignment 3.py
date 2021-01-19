# Load in packages
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

# TASK 1
# a)
df = pd.read_csv("http://www.stats.gla.ac.uk/~levers/dpip/airquality.csv")

# b)
for i, g in df.groupby('AirQualityStationEoICode'):
     g.to_csv('{}.csv'.format(i.split('/')[0]), index=False)

# c)
sort_df = df.sort_values('Concentration', ascending=False)
date_station = sort_df.iloc[0:1, 0:5]
date = pd.to_datetime(date_station[['Year', 'Month', 'Day']])
station = date_station['AirQualityStationEoICode']
print(date)
print(station)

# d)
group_multiple_df = df.groupby(['AirQualityStationEoICode', 'Day',
                                'Month'], as_index=False).agg({'Concentration': ['mean']})
print(group_multiple_df)

# e)
Exc_df = df.groupby('AirQualityStationEoICode', as_index=False)['Concentration'].mean()
group_multiple_df = group_multiple_df.rename(columns={'AirQualityStationEoICode/': 'AirQualityStationEoICode'})
Exc_df_fin = pd.merge(left=Exc_df, right=group_multiple_df,
                      left_on='AirQualityStationEoICode', right_on='AirQualityStationEoICode/')

# f)
December_df = df.query('Month == 12')

# g)
boxplot1 = sns.boxplot(x="AirQualityStationEoICode", y="Concentration", data=December_df)
plt.show()

# h)
scatterplot1 = sns.scatterplot(data=December_df, x="Hour",
                               y="Concentration", hue="AirQualityStationEoICode")
plt.show()

# TASK 2
# a)
GoT_Appearances_df = pd.read_csv("http://www.stats.gla.ac.uk/~levers/dpip/appearances.csv")
GoT_Characters_df = pd.read_csv("http://www.stats.gla.ac.uk/~levers/dpip/characters.csv")
GoT_Episodes_df = pd.read_csv("http://www.stats.gla.ac.uk/~levers/dpip/episodes.csv")
GoT_Scenes_df = pd.read_csv("http://www.stats.gla.ac.uk/~levers/dpip/scenes.csv")

# b)
Stark_df = GoT_Characters_df.query('House == "Stark"')

# c)
Highest_nScenes_df = GoT_Appearances_df.groupby("Character", as_index=False).count()
max_val = Highest_nScenes_df.SceneID.max()
Character_Highest_Scenes = Highest_nScenes_df.query('SceneID == 632')

# d)
House_Scenes_df = pd.merge(left=GoT_Characters_df, right=GoT_Appearances_df,
                        left_on='Name', right_on='Character')
Grouped_House_Scenes_df = House_Scenes_df.groupby(["Character", "House"],
                                                  as_index=False).count()
Max_House_Scenes = Grouped_House_Scenes_df.groupby(['House'],
                                                   as_index=False)['SceneID',
                                                                   'Character'].max()

# e)
Robb_df = GoT_Appearances_df.query('Character == "Robb Stark"')
Robb_df.tail(n=1)

# f)
S04_df = GoT_Scenes_df[GoT_Scenes_df['EpisodeID'].str.contains('S04')]
S04_df_joined = pd.merge(left=S04_df, right=GoT_Episodes_df,
                        left_on='EpisodeID', right_on='EpisodeID')
new = S04_df_joined.groupby('Location')
ep_list = []
for row_index, row in S04_df_joined.iterrows():
    if row['Location'] == 'North of the Wall':
        ep_list.append(row['Episode'])
ep_list = set(ep_list)
Fin_List = list(ep_list)
a = []
for i in range(1, 11):
    if i in ep_list:
        a.append('Episode {}: True'.format(i))
    else:
        a.append('Episode {}: False'.format(i))
print(a)

# g)
df_1 = pd.merge(left=GoT_Scenes_df, right=GoT_Appearances_df,
                        left_on='SceneID', right_on='SceneID')
col = []
for index, row in df_1.iterrows():
    pd.DataFrame(col.append(row['SceneID'][2]))
    
df_1.EpisodeID = col
df_1 = df_1.rename(columns={'EpisodeID': 'Season'})

df_1.SceneEnd = df_1.SceneEnd - df_1.SceneStart
df_1 = df_1.rename(columns={'SceneEnd': 'ScreenTime'})

df_char_stime = df_1.groupby(['Character', 'Season'], as_index=False)['ScreenTime'].sum()
df_char_stime_wide = df_char_stime.pivot(index='Character', columns='Season',
                                         values='ScreenTime')
print(df_char_stime_wide)

df_char_stime_hier = df_char_stime.set_index(['Character', 'Season'])
print(df_char_stime_hier)

# h)
subsetDataFrame = df_char_stime[df_char_stime['Character'].isin(['Jon Snow',
                                                               'Tyrion Lannister',
                                                               'Cersei Lannister',
                                                               'Arya Stark',
                                                                 'Jamie Lannister',
                                                                 'Sansa Stark',
                                                                 'Daenerys Targaryen'])]
sns.relplot(x='Season', y='ScreenTime', hue='Character', kind='line',
            data=subsetDataFrame)
plt.show()