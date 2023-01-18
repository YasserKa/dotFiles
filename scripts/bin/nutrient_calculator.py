#!/usr/bin/env python

import pandas as pd

FOOD = {
    # "Weight", "Energy", "fat", "saturated fat", "carbohydrates", "sugar", "fiber", "protein", "salt",
    "muesli": [100, 426, 17, 1.8, 52, 3.7, 8.1, 12, 0.08],
    "yogurt": [100, 40, 0.5, 0.3, 4, 4, 0, 4.1, 0.1],
    "vanilla yogurt": [100, 70, 2, 1.3, 8.7, 8.7, 0, 3.9, 0.1],
    "brown rice": [100, 350, 1.7, 0, 77, 0, 1, 7.8, 0],
    "olive oil": [100, 850, 92, 15, 0, 0, 0, 0, 0],
    "greek yogurt": [100, 59, 0.4, 0.1, 3.6, 3.2, 0, 10, 0.36],
    "red vinegar": [25, 0, 0, 0.5, 0, 0, 0, 0.04],
    "peas": [100, 70, 1, 0.2, 7, 2, 4.9, 4.8, 0.6],
    "tuna": [100, 190, 9.9, 1.2, 0, 0, 0, 25, 1],
    "feta cheese": [100, 280, 24, 15, 0.5, 0.5, 0, 16, 3],
    "tofu": [100, 136, 7.8, 1.3, 0.6, 0.5, 0, 15, 0.01],
    "cucumber": [99, 10, 0, 0, 2, 1, 1, 1, 0],
    "walnut": [100, 654, 65.21, 6.126, 13.71, 2.61, 6.7, 15.32, 0.02],
    # Monounsaturated	8.933 g Polyunsaturated 47.174 g omega‑3 9 g omega‑6 38 g
    "almond": [100, 579, 49.9, 3.8, 21.6, 4.4, 12.5, 21.2, 0.01],
    # Monounsaturated	31.6 g Polyunsaturated	12.3 g
    "avocado": [100, 160, 14.66, 2.13, 8.53, 0.66, 6.7, 2, 0.07],  # mono 9.8 poly 1.82
    "raspberry": [100, 53, 0.65, 0, 11.94, 4.42, 6.5, 1.2, 0],
    "blueberry": [100, 57, 0.33, 0, 14.49, 9.96, 2.4, 0.74, 0.01],
    "honey": [100, 320, 0.5, 0, 79, 79, 0, 0.5, 0.015],
    "oats": [100, 365, 7, 1.2, 58, 1.3, 9, 12, 0],
    "milk": [100, 50, 1.8, 1.3, 4.8, 4.8, 0, 3.4, 0.1],
    "purple onion": [110, 44, 0.1, 0.1, 5, 5, 2, 1, 0.04],
    "carrot": [78, 30, 0, 0, 5, 2, 5, 1, 0.06],
    "tomato": [100, 18, 0, 0, 3.9, 2.6, 1.2, 0.9, 0.05],
    "cherry tomato": [100, 25, 0, 0, 6, 4, 2, 1, 0.126],
    "onion": [148, 45, 0, 0, 11, 9, 3, 1, 0.05],
    "spinach": [100, 23, 0.4, 0.1, 3.6, 0.4, 2.2, 2.9, 0.79],
    "flaxseed": [100, 534, 42, 3.7, 29, 1.6, 27, 18, 0],
    "suggested": [0, 2500, 84, 29, 373, 108, 38, 82, 2.764],
}
NUTRIENTS = [
    "Weight",
    "Energy",
    "fat",
    "saturated fat",
    "carbohydrates",
    "sugar",
    "fiber",
    "protein",
    "sodium",
]

portion = [
    0.5,
    1,
    0.85,
    0.15,  # 10
    1,
    1,
    1,
    1 / 5,
    230 / (100 * 5),
    190 / (100 * 5),
    85 / (45 * 5),
    # 100 / (30 * 5),
    2,
    # 0.24,
    0.3,
    2,
    0.05,
    0.14,
    1,
]
needed_food = [
    "spinach",
    "greek yogurt",
    "cherry tomato",
    # "avocado",
    "almond",
    # "walnut",
    "muesli",
    "vanilla yogurt",
    "brown rice",
    "olive oil",
    "tofu",
    "tomato",
    "onion",
    # "carrot",
    "oats",
    "raspberry",
    # "blueberry",
    "milk",
    "honey",
    "flaxseed",
    "suggested",
]
# walnut 3.1g
# almond 1.5g

# 20-35% of calories from poly/mono
# less than 10% from saturated

# walnut omega-3 "250-500mg"

needed_food_dict = {key: FOOD[key] for key in needed_food}

df = pd.DataFrame(needed_food_dict, index=NUTRIENTS)
df = df.multiply(portion)
df = df.round(2)
df["taking"] = df.loc[:, df.columns != "suggested"].sum(axis=1)
df["needed"] = df["suggested"] - df["taking"]
print(df)
