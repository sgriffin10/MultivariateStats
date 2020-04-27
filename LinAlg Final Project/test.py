
for determinant in range(-1,-26,-1):
    for i in range(1, 26):
        if (abs((determinant * i) % 26) == 1):
            # new_scalar = i
            print("The determinant is: {} and the scalar is {}".format(determinant,i))
            # reverse_matrix = (new_scalar * new_matrix) % 26
        else:
            # print("There is no mod.")
            continue

# print(abs(-4))

# for i in range(-1,-26,-1):
#     print(i)