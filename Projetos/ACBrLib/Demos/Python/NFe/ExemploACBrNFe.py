import ctypes
import json
import sys
import os
from ctypes import CDLL, POINTER, byref, c_bool, c_char_p, c_int, create_string_buffer, c_ulong, c_char

#Constantes de Configuração Emitente, SoftHouse e NFe
#DLL ACBrLibNFe utilizada neste projeto é 64 ST (Single Thread)
PATH_APP = os.path.dirname(os.path.abspath(__file__))
PATH_DLLS = os.path.join(PATH_APP,'ACBrLib','x64')

#Salvar a dll e dependencias dentro de ACBrLib\x64
if os.name == 'nt':  
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'ACBrNFe64.dll')
else:
    PATH_ACBR_DLL = os.path.join(PATH_DLLS, 'libacbrnfe64.so')
    
# Ajustar os paths onde está o certificado .pfx (nt = Windows ou Linux)
if os.name == 'nt':  
    PATH_CERTIFICADO = r'E:\Certificados'
else:
    PATH_CERTIFICADO = r'/home/acbr/Documentos'
        
PATH_ACBRLIB_INI        = os.path.join(PATH_APP, 'ACBrLib.INI')
PATH_LOG                = os.path.join(PATH_APP,'')
PATH_PDF                = os.path.join(PATH_APP,'PDF','')
PATH_ARQ                = os.path.join(PATH_APP,'ARQUIVOS','')
PATH_SCHEMAS            = os.path.join(PATH_APP,'Schemas','NFe')
ARQ_PFX                 = os.path.join(PATH_CERTIFICADO, 'SeuCertificado.pfx')
SENHA_PFX               = '1234'
ARQ_VENDA_INI           = os.path.join(PATH_APP, 'NFe_Teste_Emissao.ini')
LARQUIVO_XML             = "";

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
tamanho_inicial = 9096
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

#Inicializando
LRetorno = acbr_lib.NFE_Inicializar(PATH_ACBRLIB_INI.encode("utf-8"),"".encode("utf-8"))
exibeReposta('NFE_Inicializar',LRetorno)

#configurando tipo de repsosta retorno 
acbr_lib.NFE_ConfigGravarValor( "Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(2).encode("utf-8"))

#log
acbr_lib.NFE_ConfigGravarValor( "Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))      
#configurando Dados Do Emitente e SoftwareHouse
acbr_lib.NFE_ConfigGravarValor( "DFe".encode("utf-8"), "SSLCryptLib".encode("utf-8"), str(1).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "DFe".encode("utf-8"), "SSLHttpLib".encode("utf-8"), str(3).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "DFe".encode("utf-8"), "SSLXmlSignLib".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "DFe".encode("utf-8"), "UF".encode("utf-8"), 'SP'.encode("utf-8"))

acbr_lib.NFE_ConfigGravarValor( "NFE".encode("utf-8"), "Ambiente".encode("utf-8"), str(1).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFE".encode("utf-8"), "FormaEmissao".encode("utf-8"), str(0).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFE".encode("utf-8"), "ModeloDF".encode("utf-8"), str(0).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFE".encode("utf-8"), "VersaoDF".encode("utf-8"), str(3).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFE".encode("utf-8"), "SSLType".encode("utf-8"), str(5).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFE".encode("utf-8"), "PathSchemas".encode("utf-8"), PATH_SCHEMAS.encode("utf-8"))

acbr_lib.NFE_ConfigGravarValor( "DFe".encode("utf-8"), "ArquivoPFX".encode("utf-8"), ARQ_PFX.encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "DFe".encode("utf-8"), "Senha".encode("utf-8"), SENHA_PFX.encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "DANFE".encode("utf-8"), "PathPDF".encode("utf-8"), PATH_PDF.encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFe".encode("utf-8"), "SepararPorCNPJ".encode("utf-8"), str(1).encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFe".encode("utf-8"), "PathSalvar".encode("utf-8"), PATH_ARQ.encode("utf-8"))
acbr_lib.NFE_ConfigGravarValor( "NFe".encode("utf-8"), "PathNFe".encode("utf-8"), PATH_ARQ.encode("utf-8"))

#Salvando configurações ACBrLib.INI
acbr_lib.NFE_ConfigGravar( PATH_ACBRLIB_INI.encode("utf-8"));

#Define tamanho buffer resposta
def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 

def statusServicoSefaz():
    #zera o tamanho do buffer
    define_bufferResposta(0)
    #consulta status sefaz
    resultado = acbr_lib.NFE_StatusServico( sResposta, ctypes.byref(esTamanho))
    print('resultado ',resultado)
    #Define ultimo retorno baseado no buffer de resposta
    define_bufferResposta(esTamanho.value)
    #Executa Ultimo Retorno 
    LUltimoRetorno = acbr_lib.NFE_UltimoRetorno( sResposta, ctypes.byref(esTamanho)) 
    exibeReposta('NFE_UltimoRetorno',LUltimoRetorno) 
    resposta_completa = sResposta.value.decode("utf-8")
    if resultado == 0:
        print('-[RESPOSTA STATUS SERVICO]---------------------------------------')
        print(resposta_completa)
    else:
        print('Erro ao consultar SEFAZ',resultado)
    aguardar_tecla()

def imprimirPDFNFe(PathXMLCompleto):
    define_bufferResposta(0)
    #limpar lista de NFe carregadas
    resposta = acbr_lib.NFE_LimparLista();
    exibeReposta('NFE_LimparLista',resposta) 
    #Carregar XML
    resposta = acbr_lib.NFE_CarregarXML( PathXMLCompleto.encode('utf-8') )    
    exibeReposta('NFE_CarregarXML',resposta) 
    #Alterar para teste o nome do PDF
    resposta = acbr_lib.NFE_ConfigGravarValor( 'DANFE'.encode('utf-8'), 'PathPDF'.encode('utf-8'), os.path.join(PATH_APP.encode('utf-8'),'teste'.encode('utf-8')))
    resposta = acbr_lib.NFE_ConfigGravarValor( 'DANFE'.encode('utf-8'), 'NomeDocumento'.encode('utf-8'), 'pdfteste'.encode('utf-8'))
    exibeReposta('NFE_ConfigGravarValor',resposta) 
    #persistindo no INI
    resposta = acbr_lib.NFE_ConfigGravar( PATH_ACBRLIB_INI.encode("utf-8"));
    exibeReposta('NFE_ConfigGravar',resposta) 
    #imprimir
    resposta = acbr_lib.NFE_ImprimirPDF( );
    exibeReposta('NFE_ImprimirPDF',resposta) 


def emitirImprimirNFe():
    '''
    ATENÇÃO !
    O arquivo de venda esta em INI, para obter um ini válido, segue o link:
    https://acbr.sourceforge.io/ACBrLib/ModeloNFeINI.html
    '''
    #Definir tamanho da resposta
    define_bufferResposta(0)
    #Limpar a lista de NFe's carregadas
    acbr_lib.NFE_LimparLista();
    #Carregar Arquivo de venda INI
    LResposta = acbr_lib.NFE_CarregarINI( ARQ_VENDA_INI.encode('utf-8'));
    exibeReposta('NFE_CarregarINI',LResposta)
    #Assinar a NFE
    LResposta = acbr_lib.NFE_Assinar()
    exibeReposta('NFE_CarregarINI',LResposta)
    #Obter XML da NFE
    resultado = acbr_lib.NFE_Assinar( 0, sResposta, ctypes.byref(esTamanho))
    exibeReposta('NFE_Assinar',resultado)
    #gravar XML
    resultado = acbr_lib.NFE_GravarXml(0,'','');
    exibeReposta('NFE_GravarXml',resultado)
    #Enviar a NFe a SEFAZ
    resultado = acbr_lib.NFE_Enviar( 1, False   ,True      ,False   ,sResposta, ctypes.byref(esTamanho))
    exibeReposta('NFE_Enviar',resultado)
    #Define ultimo retorno baseado no buffer de resposta
    define_bufferResposta(esTamanho.value)
    #Executa Ultimo Retorno 
    LUltimoRetorno = acbr_lib.NFE_UltimoRetorno( sResposta, ctypes.byref(esTamanho)) 
    exibeReposta('NFE_UltimoRetorno',LUltimoRetorno)      
    #exibe resposta completa
    resposta_completa = sResposta.value.decode("utf-8")
    print('Resposta: ',resposta_completa)
    json_data = json.loads(resposta_completa)
    #A funcao abaixo, so para pegar do retorno o numero da nfe, vc pode informar manualmente qual o numero p ler oXML NFe35 por exemplo
    nNFe_key = next(key for key in json_data["Envio"] if key.startswith("NFe"))
    #Alimentando a Variavel Global LARQUIVO_xml com path
    LARQUIVO_XML = json_data["Envio"][nNFe_key]["NomeArq"]
    #Colocar inserir barras duplas par aser utilizado no windows em testes
    LARQUIVO_XML = LARQUIVO_XML.replace("\\", "\\\\")
    print('ArquivoXML :'+LARQUIVO_XML)
    
def getPathCertificado():
    define_bufferResposta(0)
    resposta = acbr_lib.NFE_GetPath( 0,sResposta, ctypes.byref(esTamanho),'','digiteAquiNumeroCNPJ'.encode('utf-8'),'',0)
    resposta_completa = sResposta.value.decode("utf-8")
    print('Resposta = ',resposta)
    print('Resposta completa : ', resposta_completa)
    aguardar_tecla()

def exibir_menu():
    print("--[Menu]----------------------------------------------")
    print("1. Consulta Status SEFAZ")
    print("2. Criar XML e Enviar SEFAZ")
    print("3. Imprimir NFE")
    print("4. getPath")
    print("5. Sair")
       

while True:
    exibir_menu()
    escolha = input("Escolha uma opção: ")
    if escolha == "1":
        statusServicoSefaz()       
        aguardar_tecla()
    elif escolha == "2":
        emitirImprimirNFe()
        aguardar_tecla()
    elif escolha == "3":
        imprimirPDFNFe("C:\\ACBr\\Projetos\\ACBrLib\\Demos\\Python\\NFe\\ARQUIVOS\\11777555000100\\35240911777555000100550030000000501432115908-nfe.xml")
    elif escolha == "4":
        getPathCertificado()
    elif escolha == "5":
        print("Saindo...")
        acbr_lib.NFE_Finalizar()
        break
    else:
        print("Opção inválida. Por favor, escolha uma opção válida.")    