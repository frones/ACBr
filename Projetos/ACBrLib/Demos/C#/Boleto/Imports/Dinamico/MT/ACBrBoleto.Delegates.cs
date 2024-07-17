using System;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.Boleto
{
    public sealed partial class ACBrBoleto
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConfigurarDados(IntPtr handle, string eArquivoIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_IncluirTitulos(IntPtr handle, string eArquivoIni, string eTpSaida);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_LimparLista(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_TotalTitulosLista(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_Imprimir(IntPtr handle, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ImprimirBoleto(IntPtr handle, int eIndice, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarPDF(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SalvarPDF(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarPDFBoleto(IntPtr handle, int eIndice);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SalvarPDFBoleto(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarHTML(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarRemessa(IntPtr handle, string eDir, int eNumArquivo, string eNomeArq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_GerarRemessaStream(IntPtr handle, int eNumArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_LerRetorno(IntPtr handle, string eDir, string eNomeArq);
        
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_LerRetornoStream(IntPtr handle, string ARetornoBase64, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ObterRetorno(IntPtr handle, string eDir, string eNomeArq, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_EnviarEmail(IntPtr handle, string ePara, string eAssunto, string eMensagem, string eCC);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_EnviarEmailBoleto(IntPtr handle, int eIndice, string ePara, string eAssunto, string eMensagem, string eCC);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SetDiretorioArquivo(IntPtr handle, string eDir, string eArq);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaBancos(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaCaractTitulo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaOcorrencias(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ListaOcorrenciasEX(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_TamNossoNumero(IntPtr handle, string eCarteira, string enossoNumero, string eConvenio);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_CodigosMoraAceitos(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_SelecionaBanco(IntPtr handle, string eCodBanco);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_MontarNossoNumero(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_RetornaLinhaDigitavel(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_RetornaCodigoBarras(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_EnviarBoleto(IntPtr handle, int eCodigoOperacao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Boleto_ConsultarTitulosPorPeriodo(IntPtr handle, string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<Boleto_Inicializar>("Boleto_Inicializar");
            AddMethod<Boleto_Finalizar>("Boleto_Finalizar");
            AddMethod<Boleto_Nome>("Boleto_Nome");
            AddMethod<Boleto_Versao>("Boleto_Versao");
            AddMethod<Boleto_UltimoRetorno>("Boleto_UltimoRetorno");
            AddMethod<Boleto_ConfigImportar>("Boleto_ConfigImportar");
            AddMethod<Boleto_ConfigExportar>("Boleto_ConfigExportar");
            AddMethod<Boleto_ConfigLer>("Boleto_ConfigLer");
            AddMethod<Boleto_ConfigGravar>("Boleto_ConfigGravar");
            AddMethod<Boleto_ConfigLerValor>("Boleto_ConfigLerValor");
            AddMethod<Boleto_ConfigGravarValor>("Boleto_ConfigGravarValor");
            AddMethod<Boleto_ConfigurarDados>("Boleto_ConfigurarDados");
            AddMethod<Boleto_IncluirTitulos>("Boleto_IncluirTitulos");
            AddMethod<Boleto_LimparLista>("Boleto_LimparLista");
            AddMethod<Boleto_TotalTitulosLista>("Boleto_TotalTitulosLista");
            AddMethod<Boleto_Imprimir>("Boleto_Imprimir");
            AddMethod<Boleto_ImprimirBoleto>("Boleto_ImprimirBoleto");
            AddMethod<Boleto_GerarPDF>("Boleto_GerarPDF");
            AddMethod<Boleto_SalvarPDF>("Boleto_SalvarPDF");
            AddMethod<Boleto_GerarPDFBoleto>("Boleto_GerarPDFBoleto");
            AddMethod<Boleto_SalvarPDFBoleto>("Boleto_SalvarPDFBoleto");
            AddMethod<Boleto_GerarHTML>("Boleto_GerarHTML");
            AddMethod<Boleto_GerarRemessa>("Boleto_GerarRemessa");
            AddMethod<Boleto_GerarRemessaStream>("Boleto_GerarRemessaStream");
            AddMethod<Boleto_ObterRetorno>("Boleto_ObterRetorno");
            AddMethod<Boleto_LerRetorno>("Boleto_LerRetorno");
            AddMethod<Boleto_LerRetornoStream>("Boleto_LerRetornoStream");
            AddMethod<Boleto_EnviarEmail>("Boleto_EnviarEmail");
            AddMethod<Boleto_EnviarEmailBoleto>("Boleto_EnviarEmailBoleto");
            AddMethod<Boleto_SetDiretorioArquivo>("Boleto_SetDiretorioArquivo");
            AddMethod<Boleto_ListaBancos>("Boleto_ListaBancos");
            AddMethod<Boleto_ListaCaractTitulo>("Boleto_ListaCaractTitulo");
            AddMethod<Boleto_ListaOcorrencias>("Boleto_ListaOcorrencias");
            AddMethod<Boleto_ListaOcorrenciasEX>("Boleto_ListaOcorrenciasEX");
            AddMethod<Boleto_TamNossoNumero>("Boleto_TamNossoNumero");
            AddMethod<Boleto_CodigosMoraAceitos>("Boleto_CodigosMoraAceitos");
            AddMethod<Boleto_SelecionaBanco>("Boleto_SelecionaBanco");
            AddMethod<Boleto_MontarNossoNumero>("Boleto_MontarNossoNumero");
            AddMethod<Boleto_RetornaLinhaDigitavel>("Boleto_RetornaLinhaDigitavel");
            AddMethod<Boleto_RetornaCodigoBarras>("Boleto_RetornaCodigoBarras");
            AddMethod<Boleto_EnviarBoleto>("Boleto_EnviarBoleto");
            AddMethod<Boleto_ConsultarTitulosPorPeriodo>("Boleto_ConsultarTitulosPorPeriodo");
        }
    }
}