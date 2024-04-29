import ctypes

while True:
    print("Menu:")
    print("1. Consultar cwsBrasilAPI")
    print("2. Consultar cwsReceitaWS")
    print("3. Consultar cwsCNPJWS")
    print("Digite 0 para sair")
    try:
        provedor = int(input("Escolha uma opção: "))

        if provedor == 1:
            print("Você escolheu cwsBrasilAPI.")
        elif provedor == 2:
            print("Você escolheu cwsReceitaWS.")
        elif provedor == 3:
            print("Você escolheu cwsCNPJWS.")
        else:
            print("Encerrando o programa.")
            break

        # Carregar a DLL, ajustes os paths para seu ambiente.
        acbr_lib = ctypes.CDLL(r'C:\ACBr\Projetos\ACBrLib\Demos\Python\ConsultaCNPJ\ACBrConsultaCNPJ64.dll')
        inicializa = acbr_lib.CNPJ_Inicializar(r'C:\ACBr\Projetos\ACBrLib\Demos\Python\ConsultaCNPJ\ACBrConsultaCNPJ.INI'.encode("utf-8"),"".encode("utf-8"))
        
        # https://acbr.sourceforge.io/ACBrLib/ConfiguracoesdaBiblioteca20.html
        # Vamos configurar abaixo o INI pela lib, configurando o provedor a partir da escolha no menu

        acbr_lib.CNPJ_ConfigGravarValor("ConsultaCNPJ".encode("utf-8"), "Provedor".encode("utf-8"), str(provedor).encode("utf-8"))
        
        # Definir a assinatura da função
        acbr_lib.CNPJ_Consultar.argtypes = (ctypes.c_char_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int))
        acbr_lib.CNPJ_Consultar.restype = ctypes.c_int

        # Definir variáveis para armazenar os resultados
        sResposta = ctypes.create_string_buffer(9048) 
        esTamanho = ctypes.c_int(9048)
        
        # Definir o valor do CNPJ como uma string
        cnpj_valor =  input("Digite o CNPJ (somente numeros): ")

        # Criar um buffer de string com espaço suficiente
        sCNPJ = ctypes.create_string_buffer(15)  # +1 para o caractere nulo de terminação

        # Copiar o valor do CNPJ para o buffer
        ctypes.memmove(sCNPJ, cnpj_valor.encode('utf-8'), len(cnpj_valor))

        # Chamar a função CNPJ_Versao
        resultado = acbr_lib.CNPJ_Consultar(sCNPJ, sResposta, ctypes.byref(esTamanho))

        # Verificar o resultado
        if resultado == 0:
            print(f"tamanho da resposta: {esTamanho}")
            sMensagem = ctypes.create_string_buffer(534 + 1)    
            acbr_lib.CNPJ_UltimoRetorno(sMensagem,ctypes.byref(esTamanho))
            print(sMensagem.value.decode('utf-8'))
        else:
            print(f"Falha ao consultar CNPJ. Código de erro: {resultado}")

        acbr_lib.CNPJ_Finalizar()


    except ValueError:
        print("Entrada inválida. Por favor, digite um número.")



