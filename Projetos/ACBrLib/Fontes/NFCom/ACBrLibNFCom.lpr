{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

library ACBrLibNFCom;

uses
  {$IFDEF MT}
   {$IFDEF UNIX}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
   {$ENDIF}
  {$ENDIF}
  Interfaces, Forms, SysUtils, Classes,
  {$IFDEF MT} ACBrLibNFComMT {$ELSE} ACBrLibNFComST {$ENDIF},
  ACBrLibNFComDataModule, ACBrLibNFComConfig, ACBrLibNFComBase,
  ACBrLibConfig, ACBrLibResposta, ACBrLibComum, ACBrLibConsts,
  ACBrLibDataModule, ACBrLibNFComConsts, ACBrLibNFComRespostas;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile: String;
{$ENDIF}

exports
  //Importadas ACBrLibComum
  NFCom_Inicializar,
  NFCom_Finalizar,
  NFCom_Nome,
  NFCom_Versao,
  NFCom_OpenSSLInfo,
  NFCom_UltimoRetorno,
  NFCom_ConfigImportar,
  NFCom_ConfigExportar,
  NFCom_ConfigLer,
  NFCom_ConfigGravar,
  NFCom_ConfigLerValor,
  NFCom_ConfigGravarValor,

  //Arquivos
  NFCom_CarregarXML,
  NFCom_CarregarINI,
  NFCom_ObterXml,
  NFCom_GravarXml,
  NFCom_ObterIni,
  NFCom_GravarIni,
  NFCom_CarregarEventoXML,
  NFCom_CarregarEventoINI,
  NFCom_LimparLista,
  NFCom_LimparListaEventos,
  NFCom_Assinar,
  NFCom_Validar,
  NFCom_ValidarRegrasdeNegocios,
  NFCom_VerificarAssinatura,
  NFCom_ObterCertificados,
  NFCom_GetPath,
  NFCom_GetPathEvento,

  //Serviços
  NFCom_StatusServico,
  NFCom_Enviar,
  NFCom_Consultar,
  NFCom_Cancelar,
  NFCom_EnviarEvento,
  NFCom_EnviarEmail,
  NFCom_EnviarEmailEvento,
  NFCom_Imprimir,
  NFCom_ImprimirPDF,
  NFCom_SalvarPDF,
  NFCom_ImprimirEvento,
  NFCom_ImprimirEventoPDF,
  NFCom_SalvarEventoPDF;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc';
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.

