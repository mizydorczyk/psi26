def fv(kapital, stopa, lata):
    return kapital * (1 + stopa)**lata

kapital = 5000
stopa = 0.05
lata = 1

print(fv(kapital, stopa, lata)) # 5250.0