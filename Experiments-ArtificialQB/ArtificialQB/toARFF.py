import pandas as pd
from os import listdir

def saveARFF(df, relation, name):
    f = open(name + '.arff', "w+")
    # relation
    f.write('@relation ' + relation + '\n\n')
    # attribs
    for i in df.columns:
        f.write('@attribute ' + str(i) + ' {')
        val = df[i].unique().tolist()
        val.sort()
        aux = ''
        for j in range(len(val) - 1):
            aux += str(val[j]) + ','
        aux += str(val[-1]) + '}\n'
        f.write(aux)
    # data
    f.write('\n@data\n\n')
    data = df.values
    for i in data:
        aux = ''
        for j in range(len(i) - 1):
            aux += str(i[j]) + ','
        aux += i[-1] + '\n'
        f.write(aux)
    f.close()


path = 'D:/Doctorado/Tercer Semestre/Seminario/Datos Ruidosos 9/ConjuntosRCI'
arch = [f for f in listdir(path) if f.endswith(".csv")]

for i in arch:
    print(i)
    data = pd.read_csv(path + '/' + i)
    name = i.split('.')[0]
    data = data[["IR", "MR", "FF", "CR", "CO", "OP", "ObservedClass", "Class"]]
    saveARFF(data, 'QualitativeBankruptcy', name)
