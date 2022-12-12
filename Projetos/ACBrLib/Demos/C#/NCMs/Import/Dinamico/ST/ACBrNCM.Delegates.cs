using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.NCM
{
    public sealed partial class ACBrNCM
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_DescricaoNCM(string cNCM, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Validar(string cNCM, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_BaixarLista(string cNomeArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ObterNCMs(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_BuscarPorCodigo(string cNCM, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        public delegate int NCM_BuscarPorDescricao(string cDesc, int nTipo, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<NCM_Inicializar>("NCM_Inicializar");
            AddMethod<NCM_Finalizar>("NCM_Finalizar");
            AddMethod<NCM_Nome>("NCM_Nome");
            AddMethod<NCM_Versao>("NCM_Versao");
            AddMethod<NCM_UltimoRetorno>("NCM_UltimoRetorno");
            AddMethod<NCM_ConfigImportar>("NCM_ConfigImportar");
            AddMethod<NCM_ConfigExportar>("NCM_ConfigExportar");
            AddMethod<NCM_ConfigLer>("NCM_ConfigLer");
            AddMethod<NCM_ConfigGravar>("NCM_ConfigGravar");
            AddMethod<NCM_ConfigLerValor>("NCM_ConfigLerValor");
            AddMethod<NCM_ConfigGravarValor>("NCM_ConfigGravarValor");
            AddMethod<NCM_DescricaoNCM>("NCM_DescricaoNCM");
            AddMethod<NCM_Validar>("NCM_Validar");
            AddMethod<NCM_BaixarLista>("NCM_BaixarLista");
            AddMethod<NCM_ObterNCMs>("NCM_ObterNCMs");
            AddMethod<NCM_BuscarPorCodigo>("NCM_BuscarPorCodigo");
            AddMethod<NCM_BuscarPorDescricao>("NCM_BuscarPorDescricao");

        }
    }
}