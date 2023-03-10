import js2py as js
import pandas as pd
import os

# convert javascript file containing dictionary of all service agencies
# from IRCC into a temporary Python file
js.translate_file('../data/services-info-20221215a.js', 'temp.py')

# load in service information from the converted javascript file
from temp import *


def main():
	# load in Peel postal codes (for determining service agencies are in Peel)
	peel_codes = load_peel_postal_codes()

	# convert javascript file containing dictionary of all service agencies
	# in Canada into a temporary Python file
	js.translate_file('../data/services-info-20221215a.js', 'temp.py')
	
	# load in service information from the converted javascript file
	services = temp.ServicesData.to_list()

	# parse out information of interest for each organization into a list
	services_info = []
	for service in services:
		# general information
		name = service['Type']['en']
		coordinates = \
			f"({service['Coordinates']['Latitude']}, {service['Coordinates']['Longitude']})"
		address = service['Address']['en'].strip()
		city = service['City']['en']
		postal = service['Postal']['en']
		province = service['Province']['en']
		in_peel = 'yes' if postal[:3] in peel_codes else 'no'  # indicate whether service is in Peel
		website = service['Website']['en']
		email = service['Email']['en']
		phone = service['Telephone']['en']

		# service information
		citizen_test_prep = service['Services']['Citzenship_test_prep']
		help_with_daily_life = service['Services']['General']
		refugees = service['Services']['Help_gar']
		job_language_training = service['Services']['Job_lang_training']
		job_search = service['Services']['Job_search']
		lgbt_support = service['Services']['LGBTQ2']
		lang_assessment = service['Services']['Lang_assess']
		lang_training = service['Services']['Lang_training']
		other = service['Services']['Other']
		seniors = service['Services']['Seniors']
		women = service['Services']['Women']
		youth = service['Services']['Youth']

		services_info.append(
			[name, coordinates, address, city, postal, province, in_peel,
			website, email, phone, citizen_test_prep, help_with_daily_life,
			refugees, job_language_training, job_search, lgbt_support,
			lang_assessment, lang_training, other, seniors, women, youth]
		)

		# save extracted service info as pandas dataframe in results folder
		services_df = pd.DataFrame(
			services_info,
			columns = ['Name', 'Coordinates', 'Address', 'City', 'Postal',
			'Province', 'In Peel', 'Website', 'Email', 'Phone',
			'Citizen test preparation', 'Help with daily life', 'Refugee services',
			'Job language training', 'Job search support', 'LGBT services',
			'Language assessment', 'Language training', 'Other services',
			'Senior services', 'Women services', 'Youth services']
		)

		services_df.to_csv('../results/ircc_service_info.csv')


def load_peel_postal_codes():
	"""Return a list for postal codes (i.e: the first 3 characters of
	   postal codes) belonging to Peel.

	   Source: https://www.zillow.com/browse/homes/on/peel-regional-municipality/
	"""
	with open('../data/peel_postal_codes.txt', 'r') as f:
		codes = [s.strip() for s in f.readlines()[1:]]

	return codes


if __name__ == '__main__':
	main()
	os.remove('temp.py')  # remove temporary python file generated by script
