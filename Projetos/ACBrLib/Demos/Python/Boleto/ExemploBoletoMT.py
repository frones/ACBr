import ctypes
import json
import sys
import os
from ctypes import CDLL, POINTER, byref, c_bool, c_char_p, c_int, create_string_buffer, c_ulong, c_char

#Constantes de Configuração Emitente, SoftHouse e NFe
#DLL ACBrLibNFe utilizada neste projeto é 64 MT (Multi Thread)
PATH_APP = os.path.dirname(os.path.abspath(__file__))
PATH_DLLS = os.path.join(PATH_APP,'ACBrLib','x64')

#Salvar a dll e dependencias dentro de ACBrLib\x64
if os.name == 'nt':  
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'ACBrBoleto64.dll')
else:
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'libacbrboleto64.so')
        
PATH_ACBRLIB_INI        = os.path.join(PATH_APP, 'ACBrLib.INI')
PATH_LOG                = os.path.join(PATH_APP,'')


#Criando o ponteiro pra ser utilizado em MT
ponteiro = c_int()
ponteiro = POINTER(c_int)()

# Definindo a variável global
arquivo_NFE = ""

#limpar tela
def limpar_tela():
    if os.name == 'nt':  
        os.system('cls')
    else:
        os.system('clear')
        
#Exibe Resposta        
def exibeReposta(LMetodo, LNovaResposta):        
    if LNovaResposta == 0:
        print('O metodo ',LMetodo,' foi executado com sucesso ! Codigo: ',LNovaResposta)
    else:    
        print('Ocorreu um erro ao executar o metodo ',LMetodo,'! Codigo: ',LNovaResposta)  
        sys.exit(0)      

#Tamanho da resposta q pode variar entao utilize a funcao define_bufferResposta() para as suas necessidades
tamanho_inicial = 0
esTamanho = ctypes.c_ulong(tamanho_inicial)
sResposta = ctypes.create_string_buffer(tamanho_inicial)

#Função para aguardar pressionar uma tecla
def aguardar_tecla():
    input("Pressione Enter para continuar...")
    
#função verificar se o arquivo existe    
def existeArquivo(LPathNomeArquivo):
    if os.path.exists(LPathNomeArquivo):
        print('Ok....Arquivo ',LPathNomeArquivo,' Existe')
    else:    
        print('Erro..Arquivo ',LPathNomeArquivo,' Não encontrado !')    

# Verificando Existencia da path + DLL
existeArquivo(PATH_ACBR_DLL)  

#Definindo como sera carregado a lib (Win/Linux)    
if os.name == 'nt':  
    acbr_lib = ctypes.CDLL(PATH_ACBR_DLL)
else:
    acbr_lib =  ctypes.cdll.LoadLibrary(PATH_ACBR_DLL)
    
#Verificando se o INI Existe, se nao existe, será criado pela Lib
existeArquivo(PATH_ACBRLIB_INI) 

#Inicializando
LRetorno = acbr_lib.Boleto_Inicializar(byref(ponteiro), PATH_ACBRLIB_INI.encode("utf-8"))
exibeReposta('Boleto_Inicializar',LRetorno)

#configurando tipo de repsosta retorno 
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(2).encode("utf-8"))

#log
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))      
#configurando LSecaoBoletoBancoConfig
LTipoCobranca = '6'.encode("utf-8")  # Itau
acbr_lib.Boleto_ConfigGravarValor(ponteiro, 'BoletoBancoConfig'.encode("utf-8"),"TipoCobranca".encode("utf-8") ,LTipoCobranca)
#configurando LSecaoBoletoCedenteWS
LVersaoDF = "V2".encode("utf-8")
LUseCertificateHTTP = "1".encode("utf-8")
LLogNivel="4".encode("utf-8")
LPathGravarRegistro=os.path.join(PATH_APP,"").encode("utf-8")
LNomeArquivoLog="LogWS.TXT".encode("utf-8")
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "BoletoWebSevice".encode("utf-8"),"VersaoDF".encode("utf-8") ,LVersaoDF)
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "BoletoWebSevice".encode("utf-8"),"UseCertificateHTTP".encode("utf-8"),LUseCertificateHTTP)
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "BoletoWebSevice".encode("utf-8"),"LogNivel".encode("utf-8"),LLogNivel)
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "BoletoWebSevice".encode("utf-8"),"PathGravarRegistro".encode("utf-8"),LPathGravarRegistro)
acbr_lib.Boleto_ConfigGravarValor(ponteiro, "BoletoWebSevice".encode("utf-8"),"NomeArquivoLog".encode("utf-8"),LNomeArquivoLog)
LResposta = acbr_lib.Boleto_ConfigGravarValor(ponteiro, "Email".encode("utf-8"), "Senha".encode("utf-8"), "1234".encode("utf-8"))
if LResposta != 0:
    print("Erro ao gravar Senha do Email")

#Salvando configurações ACBrLib.INI
acbr_lib.Boleto_ConfigGravar(ponteiro, PATH_ACBRLIB_INI.encode("utf-8"));

#Define tamanho buffer resposta
def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 

def retorna_bufferResposta():    
    global tamanho_inicial, esTamanho, sResposta
    return tamanho_inicial, esTamanho, sResposta    

def CarregaCedente():
    LArqCedente = os.path.join(PATH_APP,"cedente.ini").encode("utf-8")
    print("Arquivo Cedente", LArqCedente)
    aguardar_tecla
    nResultado = acbr_lib.Boleto_ConfigurarDados(ponteiro, LArqCedente);
    if nResultado != 0:
        print("Erro ao carregar o arquivo cedente, codigo :",nResultado)
    else:
        print("cedente carregado com sucesso !")    
    aguardar_tecla()    

def CarregaTitulo():
    LArqTitulo = os.path.join(PATH_APP,"titulo.ini").encode("utf-8")
    print("Arquivo Cedente", LArqTitulo)
    aguardar_tecla
    nResultado = acbr_lib.Boleto_ConfigurarDados(ponteiro, LArqTitulo)
    if nResultado != 0:
        print("Erro ao carregar o arquivo titulo, codigo :",nResultado)
    else:
        print("titulo carregado com sucesso !")    
    aguardar_tecla() 
    
def ImprimeTitulo():
    nResultado = acbr_lib.Boleto_Imprimir(ponteiro,'')
    if nResultado != 0:
        print("Erro ao imprimir titulo, codigo :",nResultado)
    else:
        print("impresso sucesso !")    
    aguardar_tecla()    

def EnviarTituloAPI():
    define_bufferResposta(0)
    LTpEnvia = "0".encode("utf-8")
    nResultado = acbr_lib.Boleto_EnviarBoleto(ponteiro, 0 , sResposta, ctypes.byref(esTamanho))
    if nResultado != 0:
        print("Erro ao Enviar titulo, codigo :",nResultado)
    else:
        print("Enviado com sucesso !")    
        aguardar_tecla()            
        #Define ultimo retorno baseado no buffer de resposta
        define_bufferResposta(esTamanho.value)
        #Executa Ultimo Retorno 
        nResultado = acbr_lib.Boleto_UltimoRetorno(ponteiro, sResposta, ctypes.byref(esTamanho)) 
        resposta_completa = sResposta.value.decode("utf-8")
        if nResultado == 0:
            print("--[RespostaJson]--")
            print(resposta_completa)
        else:
            print('Erro ao ler ultimo retorno1',nResultado)
    aguardar_tecla()   


def exibir_menu():
    print("--[Menu]----------------------------------------------")
    print("1. Carrega Cedente")
    print("2. Carrega Titulo(s)")
    print("3. Imprime Boleto")
    print("4. Registra OnLine API")
    print("5. Sair")
       

while True:
    exibir_menu()
    escolha = input("Escolha uma opção: ")
    if escolha == "1":
        CarregaCedente()
    elif escolha == "2":
        CarregaTitulo()
    elif escolha == "3":
        ImprimeTitulo()
    elif escolha == "4":
        EnviarTituloAPI()
    elif escolha == "5":
        print("Saindo...")
        acbr_lib.Boleto_Finalizar(ponteiro)
        break
    else:
        print("Opção inválida. Por favor, escolha uma opção válida.")    
