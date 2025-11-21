from ehrql import create_dataset
from ehrql.tables.dwh import ckg_patients


dataset = create_dataset()
dataset.define_population((ckg_patients.cluster_type=='Sekolah') & (ckg_patients.date_of_birth>'2005-01-01'))

dataset.configure_dummy_data(population_size=1000)

dataset.cluster_type = ckg_patients.cluster_type
dataset.sex = ckg_patients.sex
dataset.date_of_birth = ckg_patients.date_of_birth
dataset.province = ckg_patients.province
dataset.state = ckg_patients.state
