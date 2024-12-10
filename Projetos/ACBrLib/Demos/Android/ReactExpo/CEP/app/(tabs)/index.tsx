import { useState } from 'react';
import { StyleSheet, TouchableOpacity, TextInput } from 'react-native';

import { Text, View } from '@/components/Themed';

import { NativeModules } from 'react-native';

const { ACBrCep } = NativeModules
export default function TabOneScreen() {

  const [cep, setCep] = useState("")
  const [result, setResult] = useState("")

  const buscar = () => {
    ACBrCep.inicializar()
      .then(data => setResult(data))
      .catch(error => console.log(error))

    console.log(cep)
    ACBrCep.buscarPorCep(cep)
      .then(data => setResult(data))
      .catch(error => setResult(error))
  }

  return (
    <View style={styles.container}>

      <View
        style={styles.body}


      >

        <TextInput
          style={styles.input}
          placeholder="Digite o valor inicial"
          multiline={false}
          onChangeText={setCep}
          value={cep}
          keyboardType="numeric"

        />

        <Text style={styles.label}>Resultado</Text>
        <TextInput

          style={styles.resultInput}
          placeholder="Resultado"
          value={result}
          onChangeText={setResult}
          multiline

        />

        <TouchableOpacity style={styles.searchButton} onPress={() => buscar()}>
          <Text style={styles.searchButtonText}>Buscar</Text>
        </TouchableOpacity>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center',
  },
  title: {
    fontSize: 20,
    fontWeight: 'bold',
  },
  header: {
    flexDirection: 'row',
    justifyContent: 'flex-end',
    padding: 32,
  },
  body: {
    width: '100%',
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center',
  },
  label: {
    fontSize: 20,
    marginBottom: 8,
  },
  input: {
    width: '80%',
    height: 50,
    padding: 10,
    fontSize: 18,
    borderWidth: 1,
    borderColor: '#ccc',
    borderRadius: 8,
    marginBottom: 16,
  },

  resultInput: {
    width: '80%',
    height: '50%',
    padding: 10,
    fontSize: 18,
    borderWidth: 1,
    borderColor: '#ccc',
    borderRadius: 8,
    marginBottom: 16,
  }
  ,
  searchButton: {
    backgroundColor: '#007AFF',
    paddingVertical: 10,
    paddingHorizontal: 20,
    borderRadius: 8,
    marginBottom: 16,
  },
  searchButtonText: {
    color: '#fff',
    fontSize: 16,
    fontWeight: 'bold',
  },
  footer: {
    flexDirection: 'row', // Alinhamento em linha
    justifyContent: 'space-around',
    padding: 16,
    borderTopWidth: 1,
    borderColor: '#ccc',
  },
  footerButton: {
    padding: 10,
    backgroundColor: 'transparent', // Botão transparente
  },
});
