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
        public delegate int NCM_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_DescricaoNCM(IntPtr handle, string cNCM, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_Validar(IntPtr handle, string cNCM, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_BaixarLista(IntPtr handle, string cNomeArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_ObterNCMs(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_BuscarPorCodigo(IntPtr handle, string cNCM, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NCM_BuscarPorDescricao(IntPtr handle, string cDesc, int nTipo, StringBuilder buffer, ref int bufferSize);

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