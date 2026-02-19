import random

def przyznaj_nagrode():
    rzut = random.randint(1, 6)
    print(f"Wylosowano: {rzut}")
    
    if rzut == 6:
        return "Super bonus!"
    elif rzut in [4, 5]:
        return "Nagroda standardowa"
    else:
        return "Brak nagrody..."

for i in range(5):
    print(przyznaj_nagrode())