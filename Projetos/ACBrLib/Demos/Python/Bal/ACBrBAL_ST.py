import os
import ctypes
from ctypes import CDLL, POINTER, byref, c_bool, c_char_p, c_int, create_string_buffer, c_ulong, c_char


PATH_APP = os.path.dirname(os.path.abspath(__file__))
#Salvar a dll e dependencias dentro de ACBrLib\x64
if os.name == 'nt':  
    PATH_ACBR_DLL = os.path.join(PATH_APP,"ACBrLib","x64" ,'ACBrBAL64.dll')
else:
    PATH_ACBR_DLL = os.path.join(PATH_APP,"ACBrLib","x64" ,'libacbrnfe64.so')

PATH_INI = os.path.join(PATH_APP,'ACBrLib.ini')

#Tamanho da resposta q pode variar entao utilize a funcao define_bufferResposta() para as suas necessidades
tamanho_inicial = 0
esTamanho = ctypes.c_ulong(tamanho_inicial)
sResposta = ctypes.create_string_buffer(tamanho_inicial)

#Define tamanho buffer resposta
def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 

if os.name == 'nt':  
    acbr_lib = ctypes.CDLL(PATH_ACBR_DLL)
else:
    acbr_lib =  ctypes.cdll.LoadLibrary(PATH_ACBR_DLL)

def LerPeso():

    #Iniciando a LIbBal
    nRetorno = acbr_lib.BAL_Inicializar(PATH_INI.encode("utf-8"), "".encode("utf-8"))
    if nRetorno != 0:
        exit(0)

    # VOCE CONFIGURAR NO INI "Porta" | "Modelo" | "Baud"
    acbr_lib.BAL_Ativar()
    #acbr_lib.BAL_SolicitarPeso(ponteiro)
    peso = ''
    LPrimeiroPeso = ''
    while True:
        nCONTINUA = 0
        while True:
            MillisecTimeOut = ctypes.c_int(1024)
            define_bufferResposta(0)
            funcao = acbr_lib.BAL_LePesoStr( MillisecTimeOut, sResposta, ctypes.byref(esTamanho))
            if funcao == 0:
                define_bufferResposta(esTamanho.value)
                acbr_lib.BAL_UltimoPesoLidoStr(sResposta,ctypes.byref(esTamanho))
                if nCONTINUA == 0:
                   LPrimeiroPeso = sResposta.value.decode("utf-8")
                   print(f"Primeiro Peso: {LPrimeiroPeso}")
                else:   
                    peso = sResposta.value.decode("utf-8")
                    print(f"Estabilizando : {peso}")

                if peso != LPrimeiroPeso:
                    LPrimeiroPeso = peso
                    nCONTINUA = 0
                else:
                    print(f"Peso Estabilizado: {peso}")
                    break
            else:
                print("Erro ao executar a lerpesoStr, codigo ",funcao)  
            nCONTINUA +=1

        if peso == LPrimeiroPeso:
            break

    acbr_lib.BAL_Desativar()
    acbr_lib.BAL_Finalizar()

def menu():
    print("ACBrBal SingleThread")
    print("-"*20)
    print("Selecione uma opção:")
    print("1. Ler Peso")
    print("2. Sair")

while True:
    menu()
    escolha = input("Digite sua escolha: ")
    if escolha == '1':
        LerPeso()
    elif escolha == '2':
        print("Saindo...")
        break
    else:
        print("Opção inválida! Tente novamente.")    