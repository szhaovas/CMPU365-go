import numpy as np

def drive(filename, n_sensors=2, n_hidden=9, n_motors=3):
    # loads file at filename and creates genome

    matrix_shape = (n_sensors + n_hidden + n_motors, n_hidden + n_motors)

    with open(filename, 'r') as f:
        # read in weights
        weights = f.readline().split(' ')[:-1]  # removes \n from the end
        weights = [float(w) for w in weights]
        weights = np.array(weights).reshape(matrix_shape)

        # read in expression
        expression = f.readline().split(' ')[:-1]  # removes \n from the end
        expression = [int(e) for e in expression]
        expression = np.array(expression).reshape(matrix_shape)

        # read in joints
        joints = f.readline().split(' ')[:-1]
        joints = [float(j) for j in joints]
        joints = np.array(joints)

    genome = {'weights': weights,
              'expression': expression,
              'joints': joints}
    return genome

if __name__ == '__main__':
    filename = 'k-test.txt'

    with open(filename, 'r') as f:
        win_rates = f.readline().split('( ')
