import ctypes
import json
import os
import sys

# Obtem a pasta do projeto
diretorio_script = os.path.dirname(os.path.abspath(__file__))

#Constantes de Configuração
#DLL ACBrLibSAT utilizada neste projeto é 64 ST (Single Thread)
PATH_DLL                = os.path.abspath(os.path.join(diretorio_script,r"ACBrLib\x64\ACBrETQ64.dll"))
PATH_ACBRLIB            = os.path.abspath(os.path.join(diretorio_script, "ACBrLib.INI"))
PATH_LOG                = os.path.abspath(os.path.join(diretorio_script, "Log"))
ARQ_LOGETQ              = os.path.abspath(os.path.join(diretorio_script, "LogETQ.TXT"))

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
resposta = acbr_lib.ETQ_Inicializar(PATH_ACBRLIB.encode("utf-8"),"".encode("utf-8"))
if resposta != 0:
    print('ETQ_Inicializar | Erro Código: ',resposta)
    sys.exit(1)

#configurando tipo de resposta retorno 
acbr_lib.ETQ_ConfigGravarValor("Principal".encode("utf-8"), "TipoResposta".encode("utf-8"), str(0).encode("utf-8"))

#Configurando o log da Biblioteca
acbr_lib.ETQ_ConfigGravarValor("Principal".encode("utf-8"), "LogNivel".encode("utf-8"), str(4).encode("utf-8"))
acbr_lib.ETQ_ConfigGravarValor("Principal".encode("utf-8"), "LogPath".encode("utf-8"), PATH_LOG.encode("utf-8"))

#função para limpar a tela
def limpar_tela():
    if os.name == 'nt':  
        os.system('cls')
    else:
        os.system('clear')
           
#Inicializar Biblioteca CEP
try:
    limpar_tela()
    acbr_lib.ETQ_InicializarETQ()
except Exception as error:
    print("Erro",error)
    
#tratar uma sequencia de bytes     
def TrataRespostaLerValor(AResposta):
    return AResposta.decode('utf-8').strip('\x00 ')

#define o tamanho de retorno do ler valor       
def DefineTamanhoLerValor(ANovoTamanho):
    global RespostaLerValor
    RespostaLerValor = ' ' * ANovoTamanho
    RespostaLerValor = RespostaLerValor.encode("utf-8")
    return RespostaLerValor
    
#Carrega informação de porta e modelo apenas para exibir nas configurações
def CarregaConfiguracao():
    global LModelo, LPorta
    define_bufferResposta(10)
    DefineTamanhoLerValor(10)
    resultado = acbr_lib.ETQ_ConfigLerValor("ETQ".encode("utf-8"), "Modelo".encode("utf-8"), RespostaLerValor, ctypes.byref(esTamanho))
    if resultado == 0:
       LModelo = TrataRespostaLerValor(RespostaLerValor);
    else: 
        print('Erro ao lerValor Modelo',resultado)    

    define_bufferResposta(100)
    DefineTamanhoLerValor(100)
    resultado = acbr_lib.ETQ_ConfigLerValor("ETQ".encode("utf-8"), "Porta".encode("utf-8"), RespostaLerValor, ctypes.byref(esTamanho))
    if resultado == 0:
       LPorta = TrataRespostaLerValor(RespostaLerValor);
    else: 
        print('Erro ao lerValor Porta',resultado)   
    return LModelo, LPorta

#Função para Configuração de porta.
def configPorta():
    limpar_tela()
    LModelo, LPorta = CarregaConfiguracao()
    print(LPorta)
    print(
    '''
    --------------------------------------------------------------------------
    EXEMPLO: ACBrLib para Imprimir Etiquetas PPLA/PPLB/ZPLII/EPL2
    ----------------------------- Configuração -------------------------------
    
    Exemplos: Com1, LPT1, USB, raw:argox, \\servidorimpressao\argox
    
    '''
    )
    LPorta   = str(input(f'Digite a Porta (Porta Atual = {LPorta}) :'))
    #Gravar as informações no arquivo ACBrLib
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "Porta".encode("utf-8"), LPorta.encode("utf-8"))
    

#função para configurar modelo
def configModelo():
    limpar_tela()
    LModelo, LPorta = CarregaConfiguracao()
    print(
    '''
    --------------------------------------------------------------------------1
    EXEMPLO: ACBrLib para Imprimir Etiquetas PPLA/PPLB/ZPLII/EPL2
    ----------------------------- Configuração -------------------------------
    '''
    )
    print(
    '''
    Modelos disponíveis (linguagem da impressora):
    [0] = etqNenhum     [1] = etqPpla     [2] = etqPplb     [3] = etqZPLII     [4] = etqEpl2
    '''
    )
    LModelo = str(input(f'Digite o código do Modelo (Modelo Atual = {LModelo}):'))
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "Modelo".encode("utf-8"), LModelo.encode("utf-8"))
    
    
#função para configurar otras opções como pagina de codigo, DPI e Margem
def configDiversas():
    limpar_tela()
    print(
    '''
    --------------------------------------------------------------------------
    EXEMPLO: ACBrLib para Imprimir Etiquetas PPLA/PPLB/ZPLII/EPL2
    ----------------------------- Configuração -------------------------------
    '''
    )
    print(
    '''
    Pagina de código disponivel:
    [0] = pceNone     [1] = pce437     [2] = pce850     [3] = pce852     
    [4] = pce860      [5] = pce1250    [6] = pce1252
    '''
    )
        
    LPaginaCodigo = str(input('Digite Código da Página de Código :'))
    print(
    '''
    DPI Disponíveis:
    [0] = dpi203     [1] = dpi300     [2] = dpi600    
    '''
    )
        
    LDpi = str(input('Digite Código de DPI :'))
    LMargemEsquerda   = str(input('Margem Esquerda :'))
    
    #Gravar as informações no arquivo ACBrLib
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "PaginaDeCodigo".encode("utf-8"),LPaginaCodigo.encode("utf-8"))
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "DPI".encode("utf-8"), LDpi.encode("utf-8"))
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "MargemEsquerda".encode("utf-8"), LMargemEsquerda.encode("utf-8"))
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "LimparMemoria".encode("utf-8"), '1'.encode("utf-8"))
    acbr_lib.ETQ_ConfigGravarValor("ETQ".encode("utf-8"), "ArqLog".encode("utf-8"), ARQ_LOGETQ.encode("utf-8"))
    
#Impressão do tipo PPLA/PPLB Unica etiqueta
def ImprimeEtqSimplesPPLAPPLB():
    LResposta = acbr_lib.ETQ_Ativar()
    if LResposta == 0:
        acbr_lib.ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G".encode('utf-8'), 0, 1)
        acbr_lib.ETQ_ImprimirTexto(0, 2,2, 1, 8, 3, "CHOC BRANCO".encode('utf-8'),0,0)
        acbr_lib.ETQ_ImprimirBarras(0,0,2,2,13,5, "7896003701685".encode('utf-8'), 10, 1)
        acbr_lib.ETQ_ImprimirTexto(0,3,3,2,18,35, "R$".encode('utf-8'),0,0 )
        acbr_lib.ETQ_ImprimirTexto(0,3,4,4,15,50, "20,59".encode('utf-8'),0,0)
        acbr_lib.ETQ_ImprimirCaixa(13,32,56,17,1,1)       
        LNumeroCopias = 1
        LAvancoEtiqueta = 600
        LResposta = acbr_lib.ETQ_Imprimir(LNumeroCopias,LAvancoEtiqueta)
        if LResposta != 0:
            print('Erro ao Imprimir, código:',LResposta)        
        LResposta = acbr_lib.ETQ_Desativar()
        if LResposta != 0:
            print('Erro ao desativar, código:',LResposta)
            
    else:
        print('Erro ao ativar, código:',LResposta)

#Impressão do tipo ZPL2/EPL2 etiqueta unica
def ImprimeEtqSimplesZPLEPL():
    LResposta = acbr_lib.ETQ_Ativar()
    LFonteT = "T".encode("utf-8")
    if LResposta == 0:
        acbr_lib.ETQ_ImprimirTextoStr( 0, 'T'.encode('utf-8'), 10,  30,  10,  10, "BISCOITO MARILAN RECH 335G".encode('utf-8'), 0, 1)
        acbr_lib.ETQ_ImprimirTextoStr( 0, 'S'.encode('utf-8'), 18,  30,  60,  10, "CHOC BRANCO".encode('utf-8'),0,0)
        acbr_lib.ETQ_ImprimirTextoStr( 0, 'T'.encode('utf-8'), 20,  30, 110,  25, "R$".encode('utf-8'))
        acbr_lib.ETQ_ImprimirTextoStr( 0, 'G'.encode('utf-8'), 30,  70, 110, 150, "20,59".encode('utf-8'))
        acbr_lib.ETQ_ImprimirTextoStr( 0, 'C'.encode('utf-8'), 1,   1,  210, 190, "Projeto ACBr - ACBrLibETQ".encode('utf-8'), 0, 1)
        acbr_lib.ETQ_ImprimirBarras(0, 0, 3,   3,  260, 180, "7898097130051".encode('utf-8'), 90, 1)
        acbr_lib.ETQ_ImprimirCaixa( 190, 10, 760,   60,  2,  2)
        LNumeroCopias = 1
        LAvancoEtiqueta = 600
        LResposta = acbr_lib.ETQ_Imprimir(LNumeroCopias,LAvancoEtiqueta)
        if LResposta != 0:
            print('Erro ao Imprimir, código:',LResposta)        
        LResposta = acbr_lib.ETQ_Desativar()
        if LResposta != 0:
            print('Erro ao desativar, código:',LResposta)            
    else:
        print('Erro ao ativar, código:',LResposta)
        
#impressão de etiquetas 3 colunas PPLA/PPLB
def ImprimirEtq3ColunasPPLAPPLB():        
    LResposta = acbr_lib.ETQ_Ativar()
    if LResposta == 0:
        acbr_lib.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 3, "BISCOITO REC 33G".encode('utf8'));
        acbr_lib.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 3, "CHOC BRANCO".encode('utf8'));
        acbr_lib.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685".encode('utf8'), 10);

        acbr_lib.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 32, "BISCOITO RECH 33G".encode('utf8'));
        acbr_lib.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 32, "CHOC BRANCO".encode('utf8'));
        acbr_lib.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685".encode('utf8'), 10);

        acbr_lib.ETQ_ImprimirTexto(0, 2, 1, 2, 2, 61, "BISCOITO RECH 33G".encode('utf8'));
        acbr_lib.ETQ_ImprimirTexto(0, 2, 1, 1, 6, 61, "CHOC BRANCO".encode('utf8'));
        acbr_lib.ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685".encode('utf8'), 10);   
        
        LNumeroCopias = 1
        LAvancoEtiqueta = 600
        LResposta = acbr_lib.ETQ_Imprimir(LNumeroCopias,LAvancoEtiqueta)
        if LResposta != 0:
            print('Erro ao Imprimir, código:',LResposta)        
        LResposta = acbr_lib.ETQ_Desativar()
        if LResposta != 0:
            print('Erro ao desativar, código:',LResposta)            
    else:
        print('Erro ao ativar, código:',LResposta)
        
#impressão de etiquetas 3 colunas ZPL2/EPL2
def ImprimirEtq3ColunasZPLEPL():            
    LResposta = acbr_lib.ETQ_Ativar()
    if LResposta == 0:
        #Impressao da primeira coluna etiqueta
        acbr_lib.ETQ_ImprimirTextoStr(0, "B".encode('utf-8'), 5, 5, 10, 5, "BISCOITO REC 33G".encode('utf-8'));
        acbr_lib.ETQ_ImprimirTextoStr(0, "C".encode('utf-8'), 1, 1, 40, 5, "CHOC BRANCO".encode('utf-8'));
        acbr_lib.ETQ_ImprimirTextoStr(0, "T".encode('utf-8'), 1, 1, 84, 5, "R$ 2,89".encode('utf-8'));
        acbr_lib.ETQ_ImprimirBarras(0, 0, 2, 2, 160,  40, "7896003701685".encode('utf-8'), 50,1);
        #Impressao da segunda coluna etiqueta
        acbr_lib.ETQ_ImprimirTextoStr(0, "B".encode('utf-8'), 5, 5, 10, 350, "BISCOITO REC 33G".encode('utf-8'));
        acbr_lib.ETQ_ImprimirTextoStr(0, "C".encode('utf-8'), 1, 1, 40, 350, "CHOC BRANCO".encode('utf-8'));
        acbr_lib.ETQ_ImprimirTextoStr(0, "T".encode('utf-8'), 1, 1, 84, 350, "R$ 2,89".encode('utf-8'));
        acbr_lib.ETQ_ImprimirBarras(0, 0, 2, 2, 160, 380, "7896003701685".encode('utf-8'), 50,1);
        #Impressao da terceira coluna etiqueta
        acbr_lib.ETQ_ImprimirTextoStr(0, "B".encode('utf-8'), 5, 5, 10, 700, "BISCOITO REC 33G".encode('utf-8'));
        acbr_lib.ETQ_ImprimirTextoStr(0, "C".encode('utf-8'), 1, 1, 40, 700, "CHOC BRANCO".encode('utf-8'));
        acbr_lib.ETQ_ImprimirTextoStr(0, "T".encode('utf-8'), 1, 1, 84, 700, "R$ 2,89".encode('utf-8'));
        acbr_lib.ETQ_ImprimirBarras(0, 0, 2, 2, 160, 730, "7896003701685".encode('utf-8'), 50,1);
        
        LNumeroCopias = 1
        LAvancoEtiqueta = 600
        LResposta = acbr_lib.ETQ_Imprimir(LNumeroCopias,LAvancoEtiqueta)
        if LResposta != 0:
            print('Erro ao Imprimir, código:',LResposta)        
        LResposta = acbr_lib.ETQ_Desativar()
        if LResposta != 0:
            print('Erro ao desativar, código:',LResposta)            
    else:
        print('Erro ao ativar, código:',LResposta)
    
#impressao etiqueta simples , seleciona o modelo
def ImpressaoETQSimples():
    LModelo, LPorta = CarregaConfiguracao()
    if LModelo == 0:
        print('Nenhum modelo foi configurado !')
    elif int(LModelo) in [1,2]: # PPLA, PPLB
        ImprimeEtqSimplesPPLAPPLB()
    else:
        ImprimeEtqSimplesZPLEPL()

#impressao etiqueta simples , seleciona o modelo
def ImpressaoETQ3Colunas():
    LModelo, LPorta = CarregaConfiguracao()
    if LModelo == 0:
        print('Nenhum modelo foi configurado !')
    elif int(LModelo) in [1,2]: # PPLA, PPLB
        ImprimirEtq3ColunasPPLAPPLB()
    else:
        ImprimirEtq3ColunasZPLEPL()
    

LOpcao = 0
while LOpcao != 6:
    limpar_tela()
    print('''
    --------------------------------------------------------------------------
    EXEMPLO: ACBrLib para Imprimir Etiquetas PPLA/PPLB/ZPLII/EPL2
    -----------------------------Menu Principal-------------------------------
    [1] Configurar Modelo
    [2] Configurar Porta
    [3] Configurações Diversas
    [4] Imprime Simples
    [5] Imprime 3 Colunas
 
    [6] Sair
    ''')
    LOpcao = int(input('Selecione a opção :'))
    if LOpcao == 1:
        configModelo()
    elif LOpcao == 2:    
        configPorta()
    elif LOpcao == 3:    
        configDiversas()
    elif LOpcao == 4:    
        ImpressaoETQSimples()
    elif LOpcao == 5:
        ImpressaoETQ3Colunas()    
       
    input('Pressione qualquer tecla...')
    limpar_tela()
    
print('Obrigado por executar o teste :)')
resultado = acbr_lib.ETQ_Finalizar()