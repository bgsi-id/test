from ehrql import create_dataset
from ehrql.tables.dwh import patients

# --dataset creation
dataset = create_dataset()
dataset.define_population(patients.age_at_recruitment > 30)
dataset.configure_dummy_data(population_size=10)

dataset.age = patients.age_at_recruitment
dataset.sex = patients.sex
dataset.hospital_name = patients.hospital_name
dataset.participant_type = patients.participant_type