{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrLibeSocialMT;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum;

function eSocial_Inicializar (var libHandle: PLibHandle; eArqConfig, eChaveCrypt: Pchar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_Finalizar (libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_Nome (const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_Versao (const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_UltimoRetorno (const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigImportar (const libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigExportar (const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigLer (const libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigGravar (const libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigLerValor (const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigGravarValor (const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_CriarEventoeSocial (const libHandle: PLibHandle; const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_EnviareSocial (const libHandle: PLibHandle; aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultareSocial (const libHandle: PLibHandle; eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_CriarEnviareSocial (const libHandle: PLibHandle; const eArqIni: PChar; aGrupo:integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_LimpareSocial (const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_CarregarXMLEventoeSocial (const libHandle: PLibHandle; const eArquivoOuXML: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetIDEmpregador (const libHandle: PLibHandle; const aIdEmpregador: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetIDTransmissor (const libHandle: PLibHandle; const aIdTransmissor: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetTipoEmpregador (const libHandle: PLibHandle; aTipoEmpregador: integer):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetVersaoDF (const libHandle: PLibHandle; const sVersao: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultaIdentificadoresEventosEmpregador (const libHandle: PLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultaIdentificadoresEventosTabela (const libHandle: PLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultaIdentificadoresEventosTrabalhador (const libHandle: PLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_DownloadEventos (const libHandle: PLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ObterCertificados(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_Validar(const libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

implementation

Uses
  ACBrLibConsts, ACBrLibeSocialBase;

function eSocial_Inicializar(var libHandle: PLibHandle; eArqConfig, eChaveCrypt: Pchar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibeSocial, eArqConfig, eChaveCrypt);
end;

function eSocial_Finalizar (libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
  libHandle := Nil;
end;

function eSocial_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function eSocial_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function eSocial_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(libHandle, sOpenSSLInfo, esTamanho);
end;

function eSocial_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function eSocial_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function eSocial_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint
  ): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function eSocial_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function eSocial_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function eSocial_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function eSocial_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;

function eSocial_CriarEventoeSocial(const libHandle: PLibHandle; const eArqIni: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
  VerificarLibInicializada(libHandle);
  Result := TACBrLibeSocial(libHandle^.Lib).CriarEventoeSocial(eArqIni);
  except
  on E: EACBrLibException do
     Result := E.Erro;

  on E: Exception do
     Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_EnviareSocial(const libHandle: PLibHandle; aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).EnviareSocial(aGrupo, sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultareSocial(const libHandle: PLibHandle; eProtocolo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).ConsultareSocial(eProtocolo, sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_CriarEnviareSocial (const libHandle:PLibHandle; const eArqIni: PChar; aGrupo:integer): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).CriarEnviareSocial(eArqIni, aGrupo);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_LimpareSocial(const libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).LimpareSocial;
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_CarregarXMLEventoeSocial (const libHandle:PLibHandle; const eArquivoOuXML: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).CarregarXMLEventoeSocial(eArquivoOuXML);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetIDEmpregador (const libHandle:PLibHandle; const aIdEmpregador: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).SetIDEmpregador(aIdEmpregador);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetIDTransmissor (const libHandle:PLibHandle; const aIdTransmissor: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).SetIDTransmissor(aIdTransmissor);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetTipoEmpregador (const libHandle:PLibHandle; aTipoEmpregador: integer):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).SetTipoEmpregador(aTipoEmpregador);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetVersaoDF (const libHandle:PLibHandle; const sVersao: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).SetVersaoDF(sVersao);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultaIdentificadoresEventosEmpregador (const libHandle:PLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).ConsultaIdentificadoresEventosEmpregador(aIdEmpregador, aTipoEvento, aPeriodoApuracao, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultaIdentificadoresEventosTabela (const libHandle:PLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).ConsultaIdentificadoresEventosTabela(aIdEmpregador, aTipoEvento, aChave, aDataInicial, aDataFinal, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultaIdentificadoresEventosTrabalhador (const libHandle:PLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).ConsultaIdentificadoresEventosTrabalhador(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_DownloadEventos (const libHandle:PLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(libHandle);
   Result := TACBrLibeSocial(libHandle^.Lib).DownloadEventos(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ObterCertificados (const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibeSocial(libHandle^.Lib).ObterCertificados(sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_Validar(const libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibeSocial(libHandle^.Lib).Validar;
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

end.

