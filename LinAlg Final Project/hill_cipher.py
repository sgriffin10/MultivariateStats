import numpy as np
import math

def getKeyMatrix(int_input):
    key_size_squared = int_input ** 22
    a = np.array([])
    a = [[0] * int_input for i in range(int_input)]
    # print(a)
    for i in range(int_input):
        for j in range(int_input):
            a[i][j] = input("For row {}, column {} of the key matrix, enter the number : ".format(i,j))
    return a

def string_to_int(message_input, key):
    message_values = []
    for letter in message_input:
        letter = ord(letter) - 65
        message_values.append(letter)
    message_values = np.asarray(message_values,dtype=int)
    print("The first vector for message values are {} .".format(message_values[0:2]))
    print("The first vector for message values are {} .".format(message_values[2:4]))
    encryption(message_values, key)

def encryption(values, key):
    key_matrix = getKeyMatrix(key)
    key_matrix = np.asarray(key_matrix,dtype=int)
    
    first_vector = key_matrix.dot(values[0:2])
    second_vector = key_matrix.dot(values[2:4])
    print("The resulting tranformed vectors are: {}{}".format(first_vector,second_vector))

    # return first_vector, second_vector

    first_vector = first_vector % 29
    second_vector = second_vector % 29
    print("The corresponding letters to those transformed vectors are: {}{}".format(first_vector, second_vector))
    
    first_vector_letter = [chr(i+65) for i in first_vector]
    second_vector_letter = [chr(i+65) for i in second_vector]
    print("The corresponding letters to the mod 26 transformed vectors are: {}{}".format(first_vector_letter, second_vector_letter))

    key_matrix_inverse(key_matrix,first_vector,second_vector)


def key_matrix_inverse(key,one,two):
    determinant = round(np.linalg.det(key))
    print("The determinant is: {}".format(determinant))

    fourth_key = key[0][0]
    new_matrix = key
    # print(new_matrix)
    new_matrix[0][0] = key[1][1]
    new_matrix[0][1] = key[0][1] * -1
    new_matrix[1][0] = key[1][0] * -1
    new_matrix[1][1] = fourth_key
    print("The inverse key matrix is: {}".format(new_matrix))

    try:
        for i in range(1, 26):
            if ((determinant * i) % 29 ==1):
                new_scalar = i
                print(i)
                reverse_matrix = (new_scalar * new_matrix) % 29
        decrypt(one,two,reverse_matrix)
    except:
        print("There is no modular multiplicative inverse.")

    
    # print(reverse_matrix)

    

def decrypt(first,second,inverse):
    first_returned = inverse.dot(first) % 29
    second_returned = inverse.dot(second) % 29
    

    first_returned_letter = [chr(i+65) for i in first_returned]
    second_returned_letter = [chr(i+65) for i in second_returned]
    joined_return = first_returned_letter + second_returned_letter
    joined_print = ''.join(joined_return)
    print("Your original message is: {}!".format(joined_print))

def main():
    key_size = 2
    message = input("Enter a 4 letter word: ")
    message = message.upper()
    string_to_int(message, key_size)

if __name__ == "__main__":
    main()