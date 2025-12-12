This is the repository for the study code of the Manuscript "Trends in incidence and prevalence of asthma in children and adolescents in Catalonia, Spain, 2010-2024: a large population-based cohort study"

Authors
Anna Palomar-Cros, Alicia Abellan, Berta Raventós, Andrea Pistillo, Carlen Reyes, Juan Luís García Rivero, Katia MC Verhamme, Elena Roel1, Talita Duarte-Salles

Affiliations
Fundació Institut Universitari per a la recerca a l'Atenció Primària de Salut Jordi Gol i Gurina (IDIAPJGol), Barcelona, Spain
Department of Medical Informatics, Erasmus University Medical Center, Rotterdam, The Netherlands.
Servicio de Neumología, Hospital de Laredo, Cantabria, Spain.

To run this code you will only need to interactuate with the CodeToRun script.
You will need to add the connection details and specifications to connect to your CDM.
The lock file contains all the package versions required to run sucesfully this code.
At the beggining of the script please run renv::activate() and renv::restore() to restore the library needed for this study.
Then, once the connection is succesfully created you will only need to run source(here::here("RunStudy.R")).
All the results for the study will be saved in the Results folder.

Note: Joinpoint modelling has been calculated using the Surveillance Research Programme of NIH: https://surveillance.cancer.gov/joinpoint/
The code in Scripts>6_AAPC only prepares the dataset to import it in the software.

Any questions please contact apalomar@idiapjgol.org 
