{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibSATClass;

interface

uses
  Classes, SysUtils, typinfo, ACBrLibMailImport,
  ACBrLibComum, ACBrLibSATDataModule;

type

  { TACBrLibPosPrinter }

  TACBrLibSAT = class(TACBrLib)
  private
    FSatDM: TLibSatDM;
    FLibMail: TACBrLibMail;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property SatDM: TLibSatDM read FSatDM;
  end;

  {%region Declaração da funções}

  {%region Redeclarando Métodos de ACBrLibComum, com nome específico}
  function SAT_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_Finalizar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_Nome(const sNome: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_Versao(const sVersao: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConfigLer(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConfigGravar(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
    var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  {%endregion}

  {%region Ativar}
  function SAT_InicializarSAT: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_DesInicializar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  {%endregion}

  {%region Funções SAT}
  function SAT_AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
    const sResposta: PChar; var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar; opcao: Integer; novoCodigo: PChar;
    const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConsultarStatusOperacional(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ConsultarNumeroSessao(cNumeroDeSessao: Integer; const sResposta: PChar;
    var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_AtualizarSoftwareSAT(const sResposta: PChar;
    var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  function SAT_ComunicarCertificadoICPBRASIL(certificado: PChar; const sResposta: PChar;
    var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  {%endregion}

  {%endregion}

implementation

uses
  sysutils,
  ACBrUtil, ACBrLibConsts, ACBrLibSATConsts, ACBrLibConfig, ACBrLibSATConfig,
  ACBrLibResposta, ACBrLibSATRespostas;

{ TACBrLibSAT }
constructor TACBrLibSAT.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibSATNome;
  fpVersao := CLibSATVersao;

  FSatDM := TLibSatDM.Create(nil);
  if FileExists(CACBrMailLIBName) then
  begin
    FLibMail := TACBrLibMail.Create(ArqConfig, ChaveCrypt);
    FSatDM.ACBrMail1 := FLibMail.GetMail;
  end;
end;

destructor TACBrLibSAT.Destroy;
begin
  FSatDM.Free;
  if(FLibMail <> nil) FLibMail.Free;

  inherited Destroy;
end;

procedure TACBrLibSAT.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibSAT.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibSAT.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibSATConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibSAT.Executar;
begin
  inherited Executar;
  FSatDM.AplicarConfiguracoes;
end;

{ ACBrLibSAT }

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function SAT_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function SAT_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function SAT_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function SAT_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function SAT_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function SAT_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function SAT_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function SAT_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Ativar}
function SAT_InicializarSAT: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_Ativar', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;
      try
        SatDM.ACBrSAT1.Inicializar;
        Result := SetRetorno(ErrOK);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_DesInicializar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('SAT_DesInicializar', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;
      try
        SatDM.ACBrSAT1.DesInicializar;
        Result := SetRetorno(ErrOK);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;
{%endregion}

{%region Funções SAT}
function SAT_AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  CNPJ, Assinatura, Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;
    CNPJ := AnsiString(CNPJvalue);
    Assinatura := AnsiString(assinaturaCNPJs);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('SAT_AssociarAssinatura(' + CNPJ + ',' + Assinatura +  ' )', logCompleto, True)
    else
      pLib.GravarLog('SAT_AssociarAssinatura', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.AssociarAssinatura(CNPJ, Assinatura);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('SAT_BloquearSAT', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.BloquearSAT;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('SAT_DesbloquearSAT', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.DesbloquearSAT;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar; opcao: Integer; novoCodigo: PChar;
    const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  CodigoAtivacao, NovoCodigoAtv, Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;
    CodigoAtivacao := AnsiString(codigoDeAtivacaoOuEmergencia);
    NovoCodigoAtv := AnsiString(novoCodigo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('SAT_TrocarCodigoDeAtivacao(' + CodigoAtivacao + ',' + IntToStr(opcao)
                        + ',' + NovoCodigoAtv +  ' )', logCompleto, True)
    else
      pLib.GravarLog('SAT_TrocarCodigoDeAtivacao', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.TrocarCodigoDeAtivacao(CodigoAtivacao, opcao, NovoCodigoAtv);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('SAT_ConsultarSAT', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.ConsultarSAT;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_ConsultarStatusOperacional(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('SAT_ConsultarStatusOperacional', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.ConsultarStatusOperacional;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_ConsultarNumeroSessao(cNumeroDeSessao: Integer; const sResposta: PChar;
    var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: Ansistring;
  Resp: TRetornoConsultarSessao;
  RespCanc: TRetornoConsultarSessaoCancelado;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('SAT_ConsultarNumeroSessao(' + IntToStr(cNumeroDeSessao) +  ' )', logCompleto, True)
    else
      pLib.GravarLog('SAT_ConsultarNumeroSessao', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        SatDM.ACBrSAT1.CFe.Clear;
        SatDM.ACBrSAT1.CFeCanc.Clear;

        Resposta := SatDM.ACBrSAT1.ConsultarNumeroSessao(cNumeroDeSessao);

        if SatDM.ACBrSAT1.Resposta.codigoDeRetorno = 6000 then
        begin
          Resp := TRetornoConsultarSessao.Create(resINI);
          try
            with SatDM.ACBrSAT1.CFe do
            begin
              Resp.nCFe := IntToStrZero(ide.nCFe,0);
              Resp.XML  := AsXMLString;
              Resp.Arquivo:= SatDM.ACBrSAT1.CFe.NomeArquivo;

              Resposta := sLineBreak + Resp.Gerar;
            end;
          finally
            Resp.Free;
          end;
        end;

        if SatDM.ACBrSAT1.Resposta.codigoDeRetorno = 7000 then
        begin
          RespCanc := TRetornoConsultarSessaoCancelado.Create(resINI);
          try
            with SatDM.ACBrSAT1.CFeCanc do
            begin
              RespCanc.nCFeCanc := IntToStrZero(ide.nCFe,0);
              RespCanc.XML  := AsXMLString;
              RespCanc.Arquivo:= SatDM.ACBrSAT1.CFe.NomeArquivo;

              Resposta := sLineBreak + Resp.Gerar;
            end;
          finally
            RespCanc.Free;
          end;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_AtualizarSoftwareSAT(const sResposta: PChar;
    var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('SAT_AtualizarSoftwareSAT', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.AtualizarSoftwareSAT;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function SAT_ComunicarCertificadoICPBRASIL(certificado: PChar; const sResposta: PChar;
    var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  cCertificado, Resposta: Ansistring;
begin
  try
    VerificarLibInicializada;
    cCertificado := Ansistring(certificado);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('SAT_ComunicarCertificadoICPBRASIL(' + cCertificado +  ' )', logCompleto, True)
    else
      pLib.GravarLog('SAT_ComunicarCertificadoICPBRASIL', logNormal);

    with TACBrLibSAT(pLib) do
    begin
      SatDM.Travar;

      try
        Resposta := '';
        Resposta := SatDM.ACBrSAT1.ComunicarCertificadoICPBRASIL(cCertificado);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        SatDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

{%endregion}

{%endregion}
end.

