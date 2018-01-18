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

unit ACBrLibNFeClass;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibNFeDataModule;

type

  { TACBrLibNFe }

  TACBrLibNFe = class(TACBrLib)
  private
    FNFeDM: TLibNFeDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: String = ''; ChaveCrypt: AnsiString = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: String = ''; ChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property NFeDM: TLibNFeDM read FNFeDM;
  end;

{%region Declaração da funções}

{%region NFe}
function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFE_CarregarININFe(const eArquivoOuINI: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFE_LimparListaNFEs: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}


{%region Servicos}
function NFE_StatusServico(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibNFeConsts, ACBrLibConfig, ACBrLibNFeConfig, ACBrNFeRespostas,
  pcnConversao, pcnAuxiliar,
  blcksock,
  ACBrUtil;

{ TACBrLibNFe }

constructor TACBrLibNFe.Create(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibNFeNome;
  fpVersao := CLibNFeVersao;

  FNFeDM := TLibNFeDM.Create(nil);
end;

destructor TACBrLibNFe.Destroy;
begin
  FNFeDM.Free;
  inherited Destroy;
end;

procedure TACBrLibNFe.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibNFe.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibNFe.CriarConfiguracao(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  fpConfig := TLibNFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFe.Executar;
begin
  inherited Executar;
  FNFeDM.AplicarConfiguracoes;
end;

{%region NFe}

function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Ok: Boolean;
  ArquivoOuXml: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  Result := 0;
  Ok := False;
  ArquivoOuXml := string(eArquivoOuXML);

  if pLib.Config.Log.Nivel > logNormal then
    pLib.GravarLog('NFE_CarregarXMLNFe(' + ArquivoOuXml + ' )', logCompleto, True)
  else
    pLib.GravarLog('NFE_CarregarXMLNFe', logNormal);

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    try
      try
        if StringEhArquivo(ArquivoOuXml) then
        begin
          if not FileExists(ArquivoOuXml) then
          begin
            Result := SetRetorno(ErrArquivoNaoExiste, Format(SErrArquivoNaoExiste, [ArquivoOuXml]));
            Exit;
          end;

          Ok := NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoOuXml);
        end
        else
          Ok := NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(ArquivoOuXml);

        if Ok then
          Result := NFeDM.ACBrNFe1.NotasFiscais.Count
        else
          Result := SetRetorno(ErrOK, SErrNFeNenhumaNFeCarregada);
      except
        on E: Exception do
        begin
          Result := SetRetorno(ErrExecutandoMetodo, E.Message);
        end
      end;
    finally
      NFeDM.Destravar;
    end;
  end;
end;

function NFE_CarregarININFe(const eArquivoOuINI: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  Result := 0;
  ArquivoOuINI := string(eArquivoOuINI);

  if pLib.Config.Log.Nivel > logNormal then
    pLib.GravarLog('NFE_CarregarININFe(' + ArquivoOuINI + ' )', logCompleto, True)
  else
    pLib.GravarLog('NFE_CarregarININFe', logNormal);

  if StringEhArquivo(ArquivoOuINI) and (not FileExists(ArquivoOuINI)) then
  begin
    Result := SetRetorno(ErrArquivoNaoExiste, Format(SErrArquivoNaoExiste, [ArquivoOuINI]));
    Exit;
  end;

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    try
      try
        if NFeDM.ACBrNFe1.NotasFiscais.LoadFromIni(ArquivoOuINI) then
          Result := NFeDM.ACBrNFe1.NotasFiscais.Count
        else
          Result := SetRetorno(ErrOK, SErrNFeNenhumaNFeCarregada);
      except
        on E: Exception do
        begin
          Result := SetRetorno(ErrExecutandoMetodo, E.Message);
        end
      end;
    finally
      NFeDM.Destravar;
    end;
  end;
end;

function NFE_LimparListaNFEs: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  pLib.GravarLog('NFE_LimparListaNFEs', logNormal);

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    try
      try
        NFeDM.ACBrNFe1.NotasFiscais.Clear;
        Result := NFeDM.ACBrNFe1.NotasFiscais.Count;
      except
        on E: Exception do
        begin
          Result := SetRetorno(ErrExecutandoMetodo, E.Message);
        end
      end;
    finally
      NFeDM.Destravar;
    end;
  end;
end;

{%endregion}

{%region Servicos}

function NFE_StatusServico(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin

  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  pLib.GravarLog('NFE_StatusServico', logNormal);

  with TACBrLibNFe(pLib) do
  begin
    NFeDM.Travar;
    Resposta := TStatusServicoResposta.Create;

    with NFeDM.ACBrNFe1 do
    begin
      try
        try
          if WebServices.StatusServico.Executar then
          begin
            Resposta.Msg := WebServices.StatusServico.Msg;
            Resposta.Versao := WebServices.StatusServico.versao;
            Resposta.TpAmb := TpAmbToStr(WebServices.StatusServico.TpAmb);
            Resposta.VerAplic := WebServices.StatusServico.VerAplic;
            Resposta.CStat := WebServices.StatusServico.CStat;
            Resposta.XMotivo := WebServices.StatusServico.XMotivo;
            Resposta.CUF := WebServices.StatusServico.CUF;
            Resposta.DhRecbto := WebServices.StatusServico.DhRecbto;
            Resposta.TMed := WebServices.StatusServico.TMed;
            Resposta.DhRetorno := WebServices.StatusServico.DhRetorno;
            Resposta.XObs := WebServices.StatusServico.XObs;

            StrPCopy(Buffer, Resposta.GerarIni);
            Result := SetRetorno(ErrOK, Buffer);
          end
          else
            Result := SetRetorno(ErrOK, SErrNFeErroStatusServico);
        except
          on E: Exception do
          begin
            Result := SetRetorno(ErrExecutandoMetodo, E.Message);
          end
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  end;
end;

{%endregion}

exports
  // Servicos
  NFE_StatusServico,

  // Arquivos
  NFE_CarregarXMLNFe,
  NFE_CarregarININFe,
  NFE_LimparListaNFEs;

initialization
  pLibClass := TACBrLibNFe;

end.
