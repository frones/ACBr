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

function eSocial_Finalizar (var libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_Nome (var libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_Versao (var libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_UltimoRetorno (var libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigImportar (var libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigExportar (var libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigLer (var libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigGravar (var libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigLerValor (var libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function eSocial_ConfigGravarValor (var libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_CriarEventoeSocial (var libHandle: PLibHandle; eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_EnviareSocial (var libHandle: PLibHandle; aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultareSocial (var libHandle: PLibHandle; eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_CriarEnviareSocial (var libHandle: PLibHandle; const eArqIni: PChar; aGrupo:integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_LimpareSocial (var libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_CarregarXMLEventoeSocial (var libHandle: PLibHandle; const eArquivoOuXML: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetIDEmpregador (var libHandle: PLibHandle; const aIdEmpregador: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetIDTransmissor (var libHandle: PLibHandle; const aIdTransmissor: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetTipoEmpregador (var libHandle: PLibHandle; aTipoEmpregador: integer):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_SetVersaoDF (var libHandle: PLibHandle; const sVersao: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultaIdentificadoresEventosEmpregador (var libHandle: PLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultaIdentificadoresEventosTabela (var libHandle: PLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_ConsultaIdentificadoresEventosTrabalhador (var libHandle: PLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

 function eSocial_DownloadEventos (var libHandle: PLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

implementation

Uses
  ACBrLibConsts, ACBrLibeSocialBase;

function eSocial_Inicializar(var libHandle: PLibHandle; eArqConfig, eChaveCrypt: Pchar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibeSocial, eArqConfig, eChaveCrypt);
end;

function eSocial_Finalizar (var libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
  pLib := Nil;
end;

function eSocial_Nome(var libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function eSocial_Versao(var libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function eSocial_UltimoRetorno(var libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function eSocial_ConfigImportar(var libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function eSocial_ConfigExportar(var libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint
  ): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function eSocial_ConfigLer(var libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function eSocial_ConfigGravar(var libHandle: PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function eSocial_ConfigLerValor(var libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function eSocial_ConfigGravarValor(var libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;

function eSocial_CriarEventoeSocial(const eArqIni: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
  VerificarLibInicializada(pLib);
  Result := TACBrLibeSocial(pLib^.Lib).CriarEventoeSocial(eArqIni);
  except
  on E: EACBrLibException do
     Result := E.Erro;

  on E: Exception do
     Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_EnviarEnviareSocial(aGrupo: integer; const sResposta: PChar; var esTamanho: longint
  ): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial(pLib^.Lib).EnviareSocial(aGrupo, sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultareSocial(eProtocolo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial(pLib^.Lib).ConsultareSocial(eProtocolo, sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_CriarEnviareSocial (const eArqIni: PChar; aGrupo:integer): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial(pLib^.Lib).CriarEnviareSocial(eArqIni, aGrupo);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_LimpareSocial: longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial(pLib^.Lib).LimpareSocial;
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_CarregarXMLEventoeSocial (const eArquivoOuXML: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.CarregarXMLEventoeSocial(eArquivoOuXML);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetIDEmpregador (const aIdEmpregador: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.SetIDEmpregador(aIdEmpregador);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetIDTransmissor (const aIdTransmissor: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.SetIDTransmissor(aIdTransmissor);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetTipoEmpregador (aTipoEmpregador: integer):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.SetTipoEmpregador(aTipoEmpregador);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_SetVersaoDF (const sVersao: PChar):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.SetVersaoDF(sVersao);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultaIdentificadoresEventosEmpregador (const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.ConsultaIdentificadoresEventosEmpregador(aIdEmpregador, aTipoEvento, aPeriodoApuracao, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultaIdentificadoresEventosTabela (const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.ConsultaIdentificadoresEventosTabela(aIdEmpregador, aTipoEvento, aChave, aDataInicial, aDataFinal, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_ConsultaIdentificadoresEventosTrabalhador (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.ConsultaIdentificadoresEventosTrabalhador(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function eSocial_DownloadEventos (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibeSocial.DownloadEventos(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, sResposta, esTamanho);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

end.

