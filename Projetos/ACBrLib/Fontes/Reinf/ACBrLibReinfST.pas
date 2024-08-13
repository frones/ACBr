{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrLibReinfST;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum;

function Reinf_Inicializar (const eArqConfig, eChaveCrypt: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_Finalizar: integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_Nome (const sNome: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_Versao (const sVersao: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_UltimoRetorno (const sMensagem: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConfigImportar (const eArqConfig: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConfigExportar (const sMensagem: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConfigLer (const eArqConfig: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConfigGravar (const eArqConfig: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConfigLerValor (const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConfigGravarValor (const eSessao, eChave, eValor: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_CriarEventoReinf (const eArqIni: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_EnviarReinf (const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConsultarReinf (eProtocolo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ConsultarReciboReinf (ePerApur: PAnsiChar; aTipoEvento: Integer; eNrInscEstab: PAnsiChar;
  eCnpjPrestador: PAnsiChar; eNrInscTomador: PAnsiChar; eDtApur: PAnsiChar;
  eCpfCnpjBenef: PAnsiChar; eCnpjFonte: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_CriarEnviarReinf (const eArqIni: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_LimparReinf: integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_CarregarXMLEventoReinf (const eArquivoOuXML: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_SetIDContribuinte (const aIdContribuinte: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_SetIDTransmissor (const aIdTransmissor: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_SetTipoContribuinte (aTipoContribuinte: integer):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_SetVersaoDF (const sVersao: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_ObterCertificados(const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function Reinf_Validar: integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

implementation

Uses
  ACBrLibConsts, ACBrLibReinfBase;

function Reinf_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibReinf, eArqConfig, eChaveCrypt);
end;

function Reinf_Finalizar: integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
  pLib := Nil;
end;

function Reinf_Nome(const sNome: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function Reinf_Versao(const sVersao: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib, sVersao, esTamanho);
end;

function Reinf_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(pLib, sOpenSSLInfo, esTamanho);
end;

function Reinf_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function Reinf_ConfigImportar(const eArqConfig: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function Reinf_ConfigExportar(const sMensagem: PAnsiChar; var esTamanho: integer
  ): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function Reinf_ConfigLer(const eArqConfig: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function Reinf_ConfigGravar(const eArqConfig: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function Reinf_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function Reinf_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

function Reinf_CriarEventoReinf(const eArqIni: PAnsiChar): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
  VerificarLibInicializada(pLib);
  Result := TACBrLibReinf(pLib^.Lib).CriarEventoReinf(eArqIni);
  except
  on E: EACBrLibException do
     Result := E.Erro;

  on E: Exception do
     Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_EnviarReinf(const sResposta: PAnsiChar; var esTamanho: integer
  ): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).EnviarReinf(sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_ConsultarReinf(eProtocolo: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).ConsultarReinf(eProtocolo, sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_ConsultarReciboReinf(ePerApur: PAnsiChar; aTipoEvento: Integer;
 eNrInscEstab: PAnsiChar; eCnpjPrestador: PAnsiChar; eNrInscTomador: PAnsiChar;
 eDtApur: PAnsiChar; eCpfCnpjBenef: PAnsiChar; eCnpjFonte: PAnsiChar;
 const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibReinf(pLib^.Lib).ConsultarReciboReinf(ePerApur,
      aTipoEvento, eNrInscEstab, eCnpjPrestador, eNrInscTomador, eDtApur,
      eCpfCnpjBenef, eCnpjFonte, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_CriarEnviarReinf (const eArqIni: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibReinf(pLib^.Lib).CriarEnviarReinf(eArqIni, sResposta, esTamanho);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_LimparReinf: integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).LimparReinf;
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_CarregarXMLEventoReinf (const eArquivoOuXML: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).CarregarXMLEventoReinf(eArquivoOuXML);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_SetIDContribuinte (const aIdContribuinte: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).SetIDContribuinte(aIdContribuinte);
  except
   on E: EACBrLibException do
    Result := E.Erro;

   on E: Exception do
    Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_SetIDTransmissor (const aIdTransmissor: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).SetIDTransmissor(aIdTransmissor);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_SetTipoContribuinte (aTipoContribuinte: integer):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).SetTipoContribuinte(aTipoContribuinte);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_SetVersaoDF (const sVersao: PAnsiChar):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
   VerificarLibInicializada(pLib);
   Result := TACBrLibReinf(pLib^.Lib).SetVersaoDF(sVersao);
  except
   on E: EACBrLibException do
   Result := E.Erro;

   on E: Exception do
   Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_ObterCertificados (const sResposta: PAnsiChar; var esTamanho: integer):integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibReinf(pLib^.Lib).ObterCertificados(sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Reinf_Validar: integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibReinf(pLib^.Lib).Validar;
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

end.

