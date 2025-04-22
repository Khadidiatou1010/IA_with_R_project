import pandas as pd
import random

# Liste des questions possibles
questions = [
    "Quel est votre âge ?", 
    "Avez-vous des enfants ?", 
    "Quel est votre travail ?", 
    "Quel est votre état civil ?", 
    "Pratiquez-vous un sport ?"
]

# Réponses possibles pour chaque question
reponses = {
    "Quel est votre âge ?": ["25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75"],
    "Avez-vous des enfants ?": ["Oui", "Non"],
    "Quel est votre travail ?": ["Ingénieur", "Médecin", "Professeur", "Avocat", "Artiste", "Sans emploi"],
    "Quel est votre état civil ?": ["Célibataire", "Marié(e)", "Divorcé(e)", "Veuf(ve)"],
    "Pratiquez-vous un sport ?": ["Oui", "Non"]
}

# Liste pour stocker les données
data = []

# Générer 2000 observations
for i in range(2000):
    question = random.choice(questions)
    answer = random.choice(reponses[question])
    data.append([question, answer])

# Créer un DataFrame
df = pd.DataFrame(data, columns=["questions", "réponses"])

# Sauvegarder le fichier CSV
df.to_csv("C:/Users/ANSD/Desktop/R Studio/IA_sur_R/base_de_vie_personne.csv", index=False)

print("Le fichier CSV a été généré avec succès.")
