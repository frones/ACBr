import ctypes
import json
import os
import sys

# Obtem a pasta do projeto
diretorio_script = os.path.dirname(os.path.abspath(__file__))

#Constantes de Configuração
#DLL ACBrLibSAT utilizada neste projeto é 64 ST (Single Thread)
PATH_DLL                = os.path.abspath(os.path.join(diretorio_script,r"ACBrLib\x64\ACBrCEP64.dll"))
PATH_ACBRLIB            = os.path.abspath(os.path.join(diretorio_script, "ACBrLib.INI"))
PATH_LOG                = os.path.abspath(os.path.join(diretorio_script, "Log"))

#Cria a pasta log se nao existir
if not os.path.exists(PATH_LOG):
   os.makedirs(PATH_LOG) 

#Verifica se a dll está no path indicado
if not os.path.exists(PATH_DLL):
   print("-------------------------------------------")
   print(f"O arquivo '{PATH_DLL}' não existe.")
   print("Verifique se a dll está no caminho indicado")
   print("-------------------------------------------")
   sys.exit(1)
   
#Validar se json 
def validar_json(json_str):
    try:
        json.loads(json_str)
        return True
    except json.decoder.JSONDecodeError:
        return False 

#Tamanho da resposta q pode variar entao utilize a funcao define_bufferResposta() para as suas necessidades
tamanho_inicial = 512
esTamanho = ctypes.c_ulong(tamanho_inicial)
sResposta = ctypes.create_string_buffer(tamanho_inicial)

#função para definir o novo temanho da resposta
def define_bufferResposta(novo_tamanho):
    global tamanho_inicial, esTamanho, sResposta
    tamanho_inicial = novo_tamanho
    esTamanho = ctypes.c_ulong(tamanho_inicial)
    sResposta = ctypes.create_string_buffer(tamanho_inicial)
    return tamanho_inicial, esTamanho, sResposta 

# Carregar a DLL, ajustes os paths para seu ambiente.
acbr_lib = ctypes.CDLL(PATH_DLL)
resposta = acbr_lib.CEP_Inicializar(PATH_ACBRLIB.encode("utf-8"),"".encode("utf-8"))
if resposta != 0:
    print('CEP_Inicializar | Erro Código: ',resposta)
    sys.exit(1)

#configurando tipo de resposta retorno 
acbr_lib.CEP_ConfigGravarValor("Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(2).encode("utf-8"))

#Configurando o log da Biblioteca
acbr_lib.CEP_ConfigGravarValor("Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.CEP_ConfigGravarValor("Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))

#função para limpar a tela
def limpar_tela():
    if os.name == 'nt':  
        os.system('cls')
    else:
        os.system('clear')
           
#Inicializar Biblioteca CEP
try:
    limpar_tela()
    acbr_lib.CEP_InicializarCEP()
except Exception as error:
    print("Erro",error)
    
#função para configurar o servico
def configuraWS():
    limpar_tela()
    print(
    '''
    --------------------------------------------------------------------------
    Como usar ACBrLib para Consultar CEP
    ----------------------------- Configuração -------------------------------
    '''
    )
    print(
    '''
    Provedores de serviços disponíveis:
    [1] = wsBuscarCep		[6] = wsKingHost    [11] = wsCorreiosSIGEP
    [2] = wsCepLivre            [7] = wsByJG        [12] = wsCepAberto
    [3] = wsRepublicaVirtual    [8] = wsCorreios    [13] = wsWSCep
    [4] = wsBases4you           [9] = wsDevMedia    [14] = wsOpenCep
    [5] = wsRNSolucoes          [10] = wsViaCep     [15] = wsBrasilAPI
    '''
    )
    LServico = str(input('Digite o código serviço :'))
    print('Caso precise de autenticação de usuario e senha:')
    LUsuario = str(input('Digite o usuario :'))
    LSenha   = str(input('Digite a Senha   :'))
    LChave   = str(input('Digite a Chave   :'))
    #Gravar as informações no arquivo ACBrLib
    acbr_lib.CEP_ConfigGravarValor("CEP".encode("utf-8"), "WebService".encode("utf-8"), LServico.encode("utf-8"))
    acbr_lib.CEP_ConfigGravarValor("CEP".encode("utf-8"), "Usuario".encode("utf-8"), LUsuario.encode("utf-8"))
    acbr_lib.CEP_ConfigGravarValor("CEP".encode("utf-8"), "Senha".encode("utf-8"), LSenha.encode("utf-8"))
    acbr_lib.CEP_ConfigGravarValor("CEP".encode("utf-8"), "ChaveAcesso".encode("utf-8"), LChave.encode("utf-8"))
    
#função para executar a consulta CEP, opção 2 do menu.
def consultaCEP():
    limpar_tela()
    LCEP = str(input('Digite o CEP :'))
    if not LCEP:
        print('ERRO',"O CEP é obrigatorio")
    else:
        define_bufferResposta(4096)
        resultado = acbr_lib.CEP_BuscarPorCEP(LCEP.encode("utf-8"), sResposta, ctypes.byref(esTamanho))
        if resultado == 0:
            if validar_json(sResposta.value.decode("utf-8")):
                dados_json = json.loads(sResposta.value.decode("utf-8"))
                print('--<Retorno JSON>------------------------------------------------------------------------------')
                print(dados_json)
                try:
                    print('--<Tratamento Retorno>------------------------------------------------------------------------')
                    print("CEP...........:",dados_json["CEP"]['Endereco1']["CEP"] )
                    print("Endereço......:",dados_json["CEP"]['Endereco1']["Logradouro"] )
                    print("Complemento...:",dados_json["CEP"]['Endereco1']["Complemento"] )
                    print("Bairro........:",dados_json["CEP"]['Endereco1']["Bairro"] )
                    print("Cidade........:",dados_json["CEP"]['Endereco1']["Municipio"] )
                    print("UF............:",dados_json["CEP"]['Endereco1']["UF"] )
                    print("IBGE Cidade...:",dados_json["CEP"]['Endereco1']["IBGE_Municipio"] )
                    print("IBGE UF.......:",dados_json["CEP"]['Endereco1']["IBGE_UF"] )
                except Exception as error:
                    print("Erro",error)               
            else:
                print('Erro ao ler resposta json, verifique o tamanho do buffer de resposta')
        else:
            print('CEP_BuscarPorCEP |Erro código',resultado)    

#Consulta por logradouro / Endereco
def consultaEncereco():
    limpar_tela()
    LTipo = str(input('Digite o tipo (AV, RUA, etc) :'))
    LEndereco = str(input('Digite o endereço :'))
    LCidade = str(input('Digite a cidade :'))
    LEstado = str(input('Digite o estado :'))
    LBairro = ''
    if not LTipo or not LEndereco or not LCidade or not LEstado:
        print('ERRO',"Campos obrigatórios !")
    else:
        define_bufferResposta(4096)
        resultado = acbr_lib.CEP_BuscarPorLogradouro(LCidade.encode("utf-8"), LTipo.encode("utf-8"), LEndereco.encode("utf-8"), LEstado.encode("utf-8"), LBairro.encode("utf-8"), sResposta, ctypes.byref(esTamanho))
        print('Código do Resultado',resultado)
        if resultado == 0:
            if validar_json(sResposta.value.decode("utf-8")):
                dados_json = json.loads(sResposta.value.decode("utf-8"))
                print('--<Retorno JSON>------------------------------------------------------------------------------')
                print(dados_json)
                try:
                    print('--<Tratamento Retorno>------------------------------------------------------------------------')
                    LQtdeEnderecos = int(dados_json["CEP"]["Quantidade"])
                    if LQtdeEnderecos > 0: 
                        for i in range(1, LQtdeEnderecos + 1):
                            chave_endereco = f"Endereco{i}"
                            print(chave_endereco,'----------------------------------------------')
                            print("CEP...........:",dados_json["CEP"][chave_endereco]["CEP"] )
                            print("Endereço......:",dados_json["CEP"][chave_endereco]["Logradouro"] )
                            print("Complemento...:",dados_json["CEP"][chave_endereco]["Complemento"] )
                            print("Bairro........:",dados_json["CEP"][chave_endereco]["Bairro"] )
                            print("Cidade........:",dados_json["CEP"][chave_endereco]["Municipio"] )
                            print("UF............:",dados_json["CEP"][chave_endereco]["UF"] )
                            print("IBGE Cidade...:",dados_json["CEP"][chave_endereco]["IBGE_Municipio"] )
                            print("IBGE UF.......:",dados_json["CEP"][chave_endereco]["IBGE_UF"] ) 
                    else:
                        print('nenhum endereço encontrado!')                                  
                except Exception as error:
                    print("Erro",error)
            else:
                print('Erro ao validar json da reposta, verifique o buffer de resposta')        
        else:
            print('CEP_BuscarPorLogradouro | Erro  código: ',resultado)    

LOpcao = 0
while LOpcao != 4:
    limpar_tela()
    print('''
    --------------------------------------------------------------------------
    Como usar ACBrLib para Consultar CEP
    -----------------------------Menu Principal-------------------------------
    [1] Configurar
    [2] Consultar CEP
    [3] Consultar Encereço
    [4] Sair
    ''')
    LOpcao = int(input('Selecione a opção :'))
    if LOpcao == 1:
        configuraWS()
    elif LOpcao == 2:    
        consultaCEP()
    elif LOpcao == 3:    
        consultaEncereco()
       
    input('Pressione qualquer tecla...')
    limpar_tela()
    
print('Obrigado por executar o teste :)')
resultado = acbr_lib.CEP_Finalizar()