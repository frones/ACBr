import ctypes
import json
import sys
import os
import base64
from ctypes import CDLL, POINTER, byref, c_bool, c_char_p, c_int, create_string_buffer, c_ulong, c_char

#DLL ACBrLibMDFe utilizada neste projeto é 64 MT (Multi Thread)

#Constantes de Configuração Emitente, SoftHouse e MDFe
PATH_APP = os.path.dirname(os.path.abspath(__file__))
PATH_DLLS = os.path.join(PATH_APP,'ACBrLib','x64')

#Salvar a dll e dependencias dentro de ACBrLib\x64
if os.name == 'nt':  
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'ACBrMDFe64.dll')
else:
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'libacbrmdfe64.so')

#Ler o certificado para armazenar no INI    
def ler_certificado_pfx(caminho_arquivo):
    # Lê o arquivo PFX em modo binário
    with open(caminho_arquivo, "rb") as arquivo_pfx:
        conteudo_pfx = arquivo_pfx.read()
    # Converte o conteúdo do arquivo para Base64
    certificado_base64 = base64.b64encode(conteudo_pfx).decode('utf-8')
    return certificado_base64    

# Ajustar os paths onde está o certificado .pfx (nt = Windows ou Linux)
if os.name == 'nt':  
    PATH_CERTIFICADO = r'E:\Certificados'
else:
    PATH_CERTIFICADO = r'/home/acbr/Documentos'

PATH_ACBRLIB_INI        = os.path.join(PATH_APP, 'ACBrLib.INI')
PATH_LOG                = os.path.join(PATH_APP,'')
PATH_SCHEMAS            = os.path.join(PATH_APP,'Schemas','MDFe')
PATH_INIServico         = os.path.join(PATH_DLLS,"ACBrMDFeServicos.ini")
PATH_SALVAR             = os.path.join(PATH_APP,"Salvos","")
#informar o nome e senha do seu certificado
ARQ_PFX                 = os.path.join(PATH_CERTIFICADO, 'NomeArquivoCertificado.pfx')
SENHA_PFX               = 'SenhaCertificado'

#Criando o ponteiro pra ser utilizado em MT
ponteiro = c_int()
ponteiro = POINTER(c_int)()

# Definindo a variável global
arquivo_NFE = ""

#Tamanho da resposta q pode variar entao utilize a funcao define_bufferResposta() para as suas necessidades
tamanho_inicial = 9096
esTamanho = ctypes.c_ulong(tamanho_inicial)
sResposta = ctypes.create_string_buffer(tamanho_inicial)

#Função para aguardar pressionar uma tecla
def aguardar_tecla():
    input("Pressione Enter para continuar...")
    
#Limpa Tela1
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
    
#função verificar se o arquivo existe    
def existeArquivo(LPathNomeArquivo):
    if os.path.exists(LPathNomeArquivo):
        print('Ok....Arquivo ',LPathNomeArquivo,' Existe')
    else:    
        print('Erro..Arquivo ',LPathNomeArquivo,' Não encontrado !')

#Exibe o path Schemas    
print("Path Schemas > ",PATH_SCHEMAS)   

# Verifica Existencia do arquivo cerificado
existeArquivo(ARQ_PFX)

# Verificando Existencia da path + DLL
existeArquivo(PATH_ACBR_DLL)

#Definindo como sera carregado a lib (Win/Linux)    
if os.name == 'nt':  
    acbr_lib = ctypes.CDLL(PATH_ACBR_DLL)
else:
    acbr_lib =  ctypes.cdll.LoadLibrary(PATH_ACBR_DLL)

#Verificando se o INI Existe, se nao existe, será criado pela Lib
existeArquivo(PATH_ACBRLIB_INI)

#Verificando se existe INI ServicoMDFe    
existeArquivo(PATH_INIServico)

#Inicializando MDFe
LRetorno = acbr_lib.MDFE_Inicializar(byref(ponteiro), PATH_ACBRLIB_INI.encode("utf-8"),"".encode("utf-8"))
exibeReposta('MDFE_Inicializar',LRetorno)

#configurando tipo de reposta retorno 
print('Configurando Retorno, Log e Certificado...')
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(2).encode("utf-8"))
#configurando log
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))      
#configurando certificado e ambiente
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "SSLCryptLib".encode("utf-8"), str(1).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "SSLHttpLib".encode("utf-8"), str(3).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "SSLXmlSignLib".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "UF".encode("utf-8"), 'SP'.encode("utf-8"))
#Utilizar arquivo PFX
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "ArquivoPFX".encode("utf-8"), ARQ_PFX.encode("utf-8"))

#Utilizar DadosPFX
#LDadosPFX = ler_certificado_pfx(ARQ_PFX)
#acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "DadosPFX".encode("utf-8"), LDadosPFX.encode("utf-8"))

acbr_lib.MDFE_ConfigGravarValor(ponteiro, "DFe".encode("utf-8"), "Senha".encode("utf-8"), SENHA_PFX.encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "MDFe".encode("utf-8"), "Ambiente".encode("utf-8"), str(1).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "MDFe".encode("utf-8"), "SSLType".encode("utf-8"), str(5).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "MDFe".encode("utf-8"), "PathSchemas".encode("utf-8"), PATH_SCHEMAS.encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "MDFe".encode("utf-8"), "VersaoDF".encode("utf-8"), str(1).encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "MDFe".encode("utf-8"), "IniServicos".encode("utf-8"), PATH_INIServico.encode("utf-8"))
acbr_lib.MDFE_ConfigGravarValor(ponteiro, "MDFe".encode("utf-8"), "PathSalvar".encode("utf-8"), PATH_SALVAR.encode("utf-8"))

#Salvando configurações ACBrLib.INI
acbr_lib.MDFE_ConfigGravar(ponteiro, PATH_ACBRLIB_INI.encode("utf-8"));

#Define tamanho buffer resposta
def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 

#Retorna Buffer Reposta
def retorna_bufferResposta():    
    global tamanho_inicial, esTamanho, sResposta
    return tamanho_inicial, esTamanho, sResposta    

def atualizar_status_nfe(novo_status):
    global arquivo_NFE
    arquivo_NFE = rf"{novo_status}".encode('utf-8')
    
def verificar_status_nfe():
    global arquivo_NFE
    return arquivo_NFE    

def consultaStatusSefaz():
    #Define buffer resposta como zero 
    define_bufferResposta(0)
    #consulta status servico MDFe
    resultado = acbr_lib.MDFE_StatusServico(ponteiro, sResposta, ctypes.byref(esTamanho))
    resposta_completa = sResposta.value.decode("utf-8")
    exibeReposta('MDFE_StatusServico',resultado)
    if resultado == 0:
        #Ajusta tamanho do buffer recebido em esTamanho
        define_bufferResposta(esTamanho.value) 
        #Executa Ultimo Retorno 
        LUltimoRetorno = acbr_lib.MDFE_UltimoRetorno(ponteiro, sResposta, ctypes.byref(esTamanho))
        exibeReposta("MDFE_UltimoRetorno",LUltimoRetorno)    
        if LUltimoRetorno == 0:
            resposta_completa = sResposta.value.decode("utf-8")
            print('-[Resposta]--')
            print(resposta_completa)
        else:
            print("Erro ao executar ultimo retorno, código:",LUltimoRetorno)    


def criarEnviarMDFeSefaz():
    #Carregar INI
    resultado = acbr_lib.MDFE_CarregarINI(ponteiro, "ArquivoMDFe.ini".encode("utf-8"));
    exibeReposta("MDFE_CarregarINI",resultado)
    #Assinar 
    resultado = acbr_lib.MDFE_Assinar(ponteiro);
    exibeReposta("MDFE_Assinar",resultado)
    #Validar 
    resultado = acbr_lib.MDFE_Validar(ponteiro);
    exibeReposta("MDFE_Validar",resultado)    
    #Enviar 
    define_bufferResposta(0)
    resultado = acbr_lib.MDFE_Enviar(ponteiro, 1, True, True, sResposta, ctypes.byref(esTamanho))
    exibeReposta("MDFE_Enviar",resultado)    
    if resultado == 0:
        #Ajusta tamanho do buffer recebido em esTamanho
        define_bufferResposta(esTamanho.value) 
        #Executa Ultimo Retorno 
        LUltimoRetorno = acbr_lib.MDFE_UltimoRetorno(ponteiro, sResposta, ctypes.byref(esTamanho))
        exibeReposta("MDFE_UltimoRetorno",LUltimoRetorno)    
        if LUltimoRetorno == 0:
            resposta_completa = sResposta.value.decode("utf-8")
            print('-[Resposta]--')
            print(resposta_completa)
        else:
            print("Erro ao executar ultimo retorno, código:",LUltimoRetorno)    
    else:
        print("Erro ao enviar MDFe, código:",resultado)  

def exibir_menu():
    #Menu opcoes 
    print("-[Menu]-----------------------------------------------------------")
    print("[1] Consulta Status SEFAZ")
    print("[2] Criar e Enviar MDFe")
    print("[3] Sair")
    
while True:
    #executa menu
    exibir_menu()
    escolha = input("Escolha uma opção: ")
    if escolha == "1":
        consultaStatusSefaz()       
        aguardar_tecla()
    if escolha == "2":
        criarEnviarMDFeSefaz()       
        aguardar_tecla()
    elif escolha == "3":
        print("Saindo...")
        acbr_lib.MDFE_Finalizar(ponteiro)
        break
    else:
        print("Opção inválida. Por favor, escolha uma opção válida.")    
        
