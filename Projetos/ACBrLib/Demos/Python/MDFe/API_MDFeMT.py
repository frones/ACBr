#Exemplo Basico de API ACBrLibMDFe MT

import ctypes
import json
import sys
from flask import Flask, jsonify
import os
os.system('sudo command_that_needs_root')
from datetime import datetime
from ctypes import CDLL, POINTER, byref, c_bool, c_char_p, c_int, create_string_buffer, c_ulong, c_char

# Inicializa a aplicação Flask
app = Flask(__name__)
Lnumero_requisicao = 0
#DLL ACBrLibMDFe utilizada neste projeto é 64 MT (Multi Thread)
#Constantes de Configuração Emitente, SoftHouse e MDFe
PATH_APP = os.path.dirname(os.path.abspath(__file__))
PATH_DLLS = os.path.join(PATH_APP,'ACBrLib','x64')
#Salvar a dll e dependencias dentro de ACBrLib\x64
if os.name == 'nt':  
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'ACBrMDFe64.dll')
else:
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'libacbrmdfe64.so')
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
# Definindo a variável global
arquivo_NFE = ""


#Tamanho da resposta q pode variar entao utilize a funcao define_bufferResposta() para as suas necessidades
tamanho_inicial = 100
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
#Define tamanho buffer resposta
def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 
  
def opcao1(numero_requisicao):
    print('--[INICIO REQUISICAO]-----------------------------------------------------')
    #Data e hora requisicao
    DataHora = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    #Criando o ponteiro pra ser utilizado em MT
    ponteiro = c_int()
    ponteiro = POINTER(c_int)()
    #Inicializando MDFe
    LRetorno = acbr_lib.MDFE_Inicializar(byref(ponteiro), PATH_ACBRLIB_INI.encode("utf-8"),"".encode("utf-8"))
    exibeReposta('MDFE_Inicializar',LRetorno)
    if LRetorno != 0:
        exibeReposta('Inicializa',LRetorno)
        return {'Versao ACBrLibMDFe ': 'Erro Inicializar', 'handle/Ponteiro ': str(ponteiro), 'Data e Hora ': DataHora }
        sys.exit(0) # encerra aAPI
    else:
        exibeReposta('Inicializa',LRetorno)
        
    #Limpa Buffer
    define_bufferResposta(0)
    #Solicita Versao
    LRetornoVERSAO = acbr_lib.MDFE_Versao(ponteiro, sResposta, ctypes.byref(esTamanho))
    if LRetornoVERSAO == 0: #Se versao OK
        print("Tamanho Buffer da Versao:",esTamanho.value)
        #define o tamanho do buffer recebido em esTamanho
        define_bufferResposta(esTamanho.value)
        #Executa Ultimo Retorno 
        LUltimoRetorno = acbr_lib.MDFE_UltimoRetorno(ponteiro, sResposta, ctypes.byref(esTamanho))
        exibeReposta("MDFE_UltimoRetorno",LUltimoRetorno)
        if LUltimoRetorno == 0:
            resposta_completa = sResposta.value.decode("utf-8")
            print(resposta_completa)
            return {'Versao ACBrLibMDFe ': resposta_completa, 'handle/Ponteiro ': str(ponteiro), 'Data e Hora ': DataHora }
            #liberar ponteiro
            ctypes.cast(ponteiro, ctypes.POINTER(ctypes.c_void_p)).contents = None
        else:
            resposta_completa = "Erro Ultimo Retorno".decode("utf-8")
            return {'Versao ACBrLibMDFe ': resposta_completa, 'handle/Ponteiro ': str(ponteiro), 'Data e Hora ': DataHora }
            
    else: #Se versao ERRO
        print("Erro, nao foi possivel ler numero da versao, codigo:",LRetornoVERSAO)
        #FinalizaLib
    LRetornoFIM = acbr_lib.MDFE_Finalizar(ponteiro)
    exibeReposta('MDFE_Finalizar',LRetornoFIM)    
    
# Define o endpoint /versao
@app.route('/versao', methods=['GET'])
def versao():
    global Lnumero_requisicao
    Lnumero_requisicao += 1
    resultado = opcao1(Lnumero_requisicao)
    return jsonify(resultado),200
# Executa a aplicação Flask
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)