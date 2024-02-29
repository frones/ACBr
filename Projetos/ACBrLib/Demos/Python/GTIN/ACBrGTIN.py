import ctypes
import os

def obter_GTIN():
    while True:
        entrada = input("Digite o codigo de barras(GTIN): ")
        
        # Verifica se a entrada possui apenas dígitos e tem 13 caracteres
        if entrada.isdigit() and len(entrada) == 13:
            return entrada
        else:
            print("Entrada inválida. Certifique-se de digitar apenas números e ter 13 caracteres.")


os.system('cls')
#codigo = '7894900709841'
codigo = obter_GTIN()

# Carregar a DLL
acbr_lib = ctypes.CDLL(r'C:\ACBrLibGTINPython\ACBrGTIN64.dll')
inicializa = acbr_lib.GTIN_Inicializar(r'C:\ACBrLibGTINPython\ACBrGTIN.INI'.encode("utf-8"),"".encode("utf-8"))

# Definir a assinatura da função
acbr_lib.GTIN_Consultar.argtypes = (ctypes.c_char_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int))
acbr_lib.GTIN_Consultar.restype = ctypes.c_int

# Definir variáveis para armazenar os resultados
sResposta = ctypes.create_string_buffer(1024) 
esTamanho = ctypes.c_int(1024)

# Criar um buffer de string com espaço suficiente
sGTIN = ctypes.create_string_buffer(14)  # +1 para o caractere nulo de terminação

# Copiar o valor do GTIN para o buffer
ctypes.memmove(sGTIN, codigo.encode('utf-8'), len(codigo))

# Chamar a função GTIN_Consultar
resultado = acbr_lib.GTIN_Consultar(sGTIN,sResposta,ctypes.byref(esTamanho))

# Verificar o resultado
if resultado == 0:
    print(f"tamanho da resposta: {esTamanho}")
    sMensagem = ctypes.create_string_buffer(534 + 1)    
    acbr_lib.GTIN_UltimoRetorno(sMensagem,ctypes.byref(esTamanho))
    print(sMensagem.value.decode('utf-8'))
else:
    print(f"Falha ao consultar GTIN. Código de erro: {resultado}")

acbr_lib.GTIN_Finalizar()