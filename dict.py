import pandas as pd
import random

# Exemple de dictionnaire avec une liste de mots anglais et leur traduction en français
english_french_dict = {
    "hello": "bonjour", "apple": "pomme", "book": "livre", "computer": "ordinateur", "house": "maison",
    "car": "voiture", "tree": "arbre", "dog": "chien", "cat": "chat", "friend": "ami", 
    "food": "nourriture", "water": "eau", "sun": "soleil", "moon": "lune", "sky": "ciel",
    "star": "étoile", "school": "école", "city": "ville", "country": "pays", "love": "amour",
    "happiness": "bonheur", "computer": "ordinateur", "table": "table", "pen": "stylo", "flower": "fleur",
    "keyboard": "clavier", "chair": "chaise", "bottle": "bouteille", "friendship": "amitié", "night": "nuit",
    "morning": "matin", "rain": "pluie", "snow": "neige", "storm": "tempête", "light": "lumière",
    "darkness": "obscurité", "peace": "paix", "work": "travail", "study": "étudier", "school": "école",
    "universe": "univers", "mountain": "montagne", "sea": "mer", "river": "rivière", "ocean": "océan",
    "desert": "désert", "forest": "forêt", "village": "village", "tiger": "tigre", "lion": "lion",
    "elephant": "éléphant", "bird": "oiseau", "fish": "poisson", "cow": "vache", "sheep": "mouton",
    "horse": "cheval", "butterfly": "papillon", "bee": "abeille", "rainbow": "arc-en-ciel", "cloud": "nuage",
    "galaxy": "galaxie", "earth": "terre", "moon": "lune", "sun": "soleil", "planet": "planète",
    "mars": "mars", "jupiter": "jupiter", "venus": "vénus", "mercury": "mercure", "saturn": "saturne",
    "neptune": "neptune", "pluto": "pluton", "space": "espace", "astronaut": "astronaute", "rocket": "fusée",
    "spaceship": "vaisseau spatial", "airplane": "avion", "train": "train", "bus": "autobus", "bicycle": "vélo",
    "walk": "marcher", "run": "courir", "jump": "sauter", "sit": "s'asseoir", "stand": "se tenir",
    "sleep": "dormir", "eat": "manger", "drink": "boire", "sing": "chanter", "dance": "danser",
    "listen": "écouter", "read": "lire", "write": "écrire", "speak": "parler", "think": "penser",
    "love": "aimer", "hate": "détester", "understand": "comprendre", "forget": "oublier", "remember": "se souvenir",
    "smile": "sourire", "cry": "pleurer", "laugh": "rire", "fight": "se battre", "argue": "disputer",
    "buy": "acheter", "sell": "vendre", "pay": "payer", "work": "travailler", "rest": "se reposer",
    "study": "étudier", "learn": "apprendre", "teach": "enseigner", "help": "aider", "give": "donner",
    "take": "prendre", "receive": "recevoir", "give": "donner", "sell": "vendre", "buy": "acheter"
    # Vous pouvez ajouter plus de mots pour atteindre 2000 paires de mots.
}

# Générer une liste de 2000 paires de mots (en double et en remaniant)
english_french_list = list(english_french_dict.items()) * 100  # Répéter pour 2000 mots

# Mélanger les paires pour les randomiser
random.shuffle(english_french_list)

# Limiter à 2000 paires de mots
english_french_list = english_french_list[:2000]

# Créer un DataFrame avec les paires de mots
df = pd.DataFrame(english_french_list, columns=["question", "response"])

# Sauvegarder en fichier CSV
df.to_csv("C:/Users/ANSD/Documents/GitHub/IA_with_R_projectenglish_french_dictionary.csv", index=False)

# Afficher un aperçu du DataFrame
print(df.head())


