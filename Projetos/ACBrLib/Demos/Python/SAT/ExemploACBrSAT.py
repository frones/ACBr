import ctypes
import json
import os

#Constantes de Configuração Emitente, SoftHouse e sat
#DLL ACBrLibSAT utilizada neste projeto é 64 ST (Single Thread)
PATH_DLL                = r'C:\ACBr\Projetos\ACBrLib\Demos\Python\SAT\DLLs\ACBrSAT64.dll'
PATH_ACBrLib            = r'C:\ACBr\Projetos\ACBrLib\Demos\Python\SAT\ACBrLib.INI'
PATH_LOG                = r'C:\ACBr\Projetos\ACBrLib\Demos\Python\SAT\Log'
CFG_CNPJSH              = '16716114000172'
CFG_NUMERO_CAIXA        = '1'
CFG_EMITENTE_CNPJ       = '27101611000182'
CFG_EMITENTE_IE         = '111111111111'
CFG_EMITENTE_REGTRIB    = '1'
CFG_SAT_ASSINATURA      ='SGR-SAT SISTEMA DE GESTAO E RETAGUARDA DO SAT'
CFG_SAT_MODELO          = '1'

#DLL do sat é 64Bits
CFG_SAT_DLL             = r'C:\ACBr\Projetos\ACBrLib\Demos\Python\SAT\DLLs\dllsat.dll'

CFG_SAT_CODIGO_ATIVACAO = '00000000'
ARQ_VENDA_INI           = r'C:\ACBr\Projetos\ACBrLib\Demos\Python\SAT\CFe.ini'
REMOVE_INTEGRADOR       = '{ "Integrador" : { "Codigo" : "", "Valor" : "" } }'

# Definindo a variável global
arquivo_sat = ""

#Tamanho da resposta q pode variar entao utilize a funcao define_bufferResposta() para as suas necessidades
tamanho_inicial = 9096
esTamanho = ctypes.c_ulong(tamanho_inicial)
sResposta = ctypes.create_string_buffer(tamanho_inicial)

# Carregar a DLL, ajustes os paths para seu ambiente.
acbr_lib = ctypes.CDLL(PATH_DLL)
inicializa = acbr_lib.SAT_Inicializar(PATH_ACBrLib.encode("utf-8"),"".encode("utf-8"))
#configurando tipo de repsosta retorno 
acbr_lib.SAT_ConfigGravarValor("Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(2).encode("utf-8"))
        
#configurando Dados Do Emitente e SoftwareHouse
acbr_lib.SAT_ConfigGravarValor("SAT".encode("utf-8"), "Modelo".encode("utf-8"), CFG_SAT_MODELO.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SAT".encode("utf-8"), "CodigoDeAtivacao".encode("utf-8"), CFG_SAT_CODIGO_ATIVACAO.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SAT".encode("utf-8"), "SignAC".encode("utf-8"), CFG_SAT_ASSINATURA.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SAT".encode("utf-8"), "NomeDLL".encode("utf-8"), CFG_SAT_DLL.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SATConfig".encode("utf-8"), "ide_numeroCaixa".encode("utf-8"), CFG_NUMERO_CAIXA.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SATConfig".encode("utf-8"), "emit_CNPJ".encode("utf-8"), CFG_EMITENTE_CNPJ.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SATConfig".encode("utf-8"), "emit_IE".encode("utf-8"), CFG_EMITENTE_IE.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SATConfig".encode("utf-8"), "emit_cRegTrib".encode("utf-8"), CFG_EMITENTE_REGTRIB.encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("SATConfig".encode("utf-8"), "ide_CNPJ".encode("utf-8"), CFG_SAT_MODELO.encode("utf-8"))
#log
acbr_lib.SAT_ConfigGravarValor("Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.SAT_ConfigGravarValor("Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))

def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 

def retorna_bufferResposta():    
    global tamanho_inicial, esTamanho, sResposta
    return tamanho_inicial, esTamanho, sResposta    

def atualizar_status_sat(novo_status):
    global arquivo_sat
    arquivo_sat = rf"{novo_status}".encode('utf-8')
    
def verificar_status_sat():
    global arquivo_sat
    return arquivo_sat    

def limpar_tela():
    if os.name == 'nt':  
        os.system('cls')
    else:
        os.system('clear')

#Inicializar SAT
acbr_lib.SAT_InicializarSAT();

def exibir_menu():
    limpar_tela()
    print("Menu:")
    print("1. Consulta SAT")
    print("2. Criar e Enviar XML")
    print("3. Imprimir Cupom")
    print("4. Sair")
    
        
def opcao1():
    define_bufferResposta(9096)
    resultado = acbr_lib.SAT_ConsultarStatusOperacional(sResposta, ctypes.byref(esTamanho))
    resposta_completa = sResposta.value.decode("utf-8")
    if resultado == 0:
        print('-----------------------------------------------------------------------------------------')
        print(resposta_completa)
    else:
        Print('Erro ao consultar SAT')

def opcao2():
    '''
    ATENÇÃO !
    O arquivo de venda esta em INI, para obter um ini válido, segue o link:
    https://acbr.sourceforge.io/ACBrLib/ModeloCFeINISimplificadovalido.html
    Você precisa alterar o cnpj da softwarehouse, assinatura, cnpj e ie do emitente.
    '''
    define_bufferResposta(30600)
    resultado = acbr_lib.SAT_CriarEnviarCFe(ARQ_VENDA_INI.encode('utf-8'), sResposta, ctypes.byref(esTamanho))
    resposta_completa = sResposta.value.decode("utf-8")
    json_string = resposta_completa.replace(REMOVE_INTEGRADOR, '')
    dados_json = json.loads(json_string)
    if dados_json['ENVIO']["CodigoDeRetorno"] == 6000:
        print("SAT Emitido com sucesso:",dados_json['ENVIO']["CodigoDeRetorno"] )
        print("Número Sessão:",dados_json['ENVIO']["NumeroSessao"] )
        print("Arquivo      :",dados_json['ENVIO']["Arquivo"] )
        atualizar_status_sat(dados_json['ENVIO']["Arquivo"])
    else:
        print("Erro ao Emitir SAT: ",dados_json['ENVIO']["CodigoDeRetorno"] )
        print("Número Sessão:",dados_json['ENVIO']["NumeroSessao"] )
        verificar_status_sat()
        
def opcao3():
    #print('Imprimir XML ',verificar_status_sat())
    if os.path.exists(verificar_status_sat()):
        resposta = acbr_lib.SAT_ImprimirExtratoVenda(verificar_status_sat());
        if resposta == 0:
            print('Impresso com sucesso !')
        else:    
            print('Erro ao imprimir ',verificar_status_sat())
    else:
        print("O arquivo não existe.",)

def aguardar_tecla():
    input("Pressione Enter para continuar...")
  
    
while True:
    exibir_menu()
    escolha = input("Escolha uma opção: ")
    if escolha == "1":
        opcao1()       
        aguardar_tecla()
    elif escolha == "2":
        opcao2()
        aguardar_tecla()
    elif escolha == "3":
        opcao3()
    elif escolha == "4":
        print("Saindo...")
        acbr_lib.SAT_Finalizar()

        break
    else:
        print("Opção inválida. Por favor, escolha uma opção válida.")    