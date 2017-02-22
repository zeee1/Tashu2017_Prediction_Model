import csv
import operator, collections
from sklearn.cluster import KMeans

rental_file = open('../../tashuData/tashu2yearsData.csv','r')
rental = csv.DictReader(rental_file)
rental = list(rental)

result_file = open('../../tashuData/resultFile.csv','w',newline='')
fieldnames = ['IS_MEMBER', 'RENT_STATION', 'RENT_DATE', 'RETURN_STATION', 'RETURN_DATE']
writer = csv.DictWriter(result_file, fieldnames = fieldnames)
writer.writeheader()

for row in rental:
	isMember = row['IS_MEMBER']
	rentDate = row['RENT_DATE']
	returnDate = row['RETURN_DATE']
	rentStation = row['RENT_STATION']
	returnStation = row['RETURN_STATION']
	if len(rentStation) == 0 or len(returnStation) == 0 or len(returnDate) == 0 or len(rentDate) == 0:
		continue

	writer.writerow(row)

print("End")