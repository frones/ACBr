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
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property NFeDM: TLibNFeDM read FNFeDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region NFe}
function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_CarregarININFe(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_LimparListaNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ValidarNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConsultarNFe(const eChaveOuNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_InutilizarNFe(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibNFeConsts, ACBrLibConfig, ACBrLibNFeConfig,
  ACBrLibNFeRespostas, ACBrNFe,
  pcnConversao, pcnAuxiliar, blcksock, ACBrUtil;

{ TACBrLibNFe }

constructor TACBrLibNFe.Create(ArqConfig: string; ChaveCrypt: ansistring);
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

procedure TACBrLibNFe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibNFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFe.Executar;
begin
  inherited Executar;
  FNFeDM.AplicarConfiguracoes;
end;

{%region NFe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_CarregarXMLNFe(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_CarregarXMLNFe', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        if EhArquivo then
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoOuXml)
        else
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(ArquivoOuXml);

        Result := SetRetornoNFesCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_CarregarININFe(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_CarregarININFe(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_CarregarININFe', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoNFesCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_LimparListaNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_LimparListaNFEs', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        NFeDM.ACBrNFe1.NotasFiscais.Clear;
        Result := SetRetornoNFesCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ValidarNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_ValidarNFEs', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        try
          NFeDM.ACBrNFe1.NotasFiscais.Validar;
        except
          on E: EACBrNFeException do
            Result := SetRetorno(ErrValidacaoNFe, E.Message);
        end;
        Result := SetRetornoNFesCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_ValidarRegrasdeNegocios', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        Erros := '';
        NFeDM.ACBrNFe1.NotasFiscais.ValidarRegrasdeNegocios(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_VerificarAssinatura', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        Erros := '';
        NFeDM.ACBrNFe1.NotasFiscais.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        NFeDM.Destravar;
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

{%region Servicos}

function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_StatusServico', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TStatusServicoResposta.Create(pLib.Config.TipoResposta);
      try
        with NFeDM.ACBrNFe1 do
        begin
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

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'StatusServico');
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_ConsultarNFe(const eChaveOuNFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ChaveOuNFe: string;
  Resposta: TConsultaNFeResposta;
begin
  try
    VerificarLibInicializada;

    ChaveOuNFe := string(eChaveOuNFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_ConsultarNFe(' + ChaveOuNFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_ConsultarNFe', logNormal);

    EhArquivo := StringEhArquivo(ChaveOuNFe);
    if EhArquivo then
      VerificarArquivoExiste(ChaveOuNFe);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;

      if EhArquivo then
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ChaveOuNFe);

      if NFeDM.ACBrNFe1.NotasFiscais.Count = 0 then
      begin
        if ValidarChave(Cmd.Params(0)) then
          NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := Cmd.Params(0)
        else
          raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [ChaveOuNFe]));
      end
      else
        NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := StringReplace(
          NFeDM.ACBrNFe1.NotasFiscais.Items[NFeDM.ACBrNFe1.NotasFiscais.Count - 1].NFe.infNFe.ID,
          'NFe','',[rfIgnoreCase]);


      Resposta := TConsultaNFeResposta.Create(pLib.Config.TipoResposta);
      try
        with NFeDM.ACBrNFe1 do
        begin
          if WebServices.Consulta.Executar then
          begin
            Resposta.Msg := WebServices.Consulta.Msg;
            Resposta.Versao := WebServices.Consulta.versao;
            Resposta.TpAmb := TpAmbToStr(WebServices.Consulta.TpAmb);
            Resposta.VerAplic := WebServices.Consulta.VerAplic;
            Resposta.CStat := WebServices.Consulta.CStat;
            Resposta.XMotivo := WebServices.Consulta.XMotivo;
            Resposta.CUF := WebServices.Consulta.CUF;
            Resposta.DhRecbto := WebServices.Consulta.DhRecbto;
            Resposta.ChNFe := WebServices.Consulta.NFeChave;
            Resposta.NProt := WebServices.Consulta.Protocolo;
            Resposta.DigVal := WebServices.Consulta.protNFe.digVal;

            MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
            Result := SetRetorno(ErrOK, StrPas(sResposta));
          end
          else
          Result := SetRetornoWebService(SSL.HTTPResultCode, 'StatusServico');
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_InutilizarNFe(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TInutilizarNFeResposta;
  CNPJ, Justificativa: string;
begin
  try
    VerificarLibInicializada;

    Justificativa := string(AJustificativa);
    CNPJ := string(ACNPJ);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_InutilizarNFe(' + CNPJ + ',' + Justificativa + ',' + IntToStr(Ano) + ',' +
        IntToStr(modelo) + ',' + IntToStr(Serie) +  ',' + IntToStr(NumeroInicial) +  ',' +
        IntToStr(NumeroFinal) + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_InutilizarNFe', logNormal);

    CNPJ := OnlyNumber(CNPJ);

    if not ValidarCNPJ(CNPJ) then
       raise EACBrNFeException.Create('CNPJ: ' + CNPJ + ', inválido.');

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TInutilizarNFeResposta.Create(pLib.Config.TipoResposta);
      try
        with NFeDM.ACBrNFe1 do
        begin
          with WebServices do
          begin
            Inutilizacao.CNPJ := CNPJ;
            Inutilizacao.Justificativa := Justificativa;
            Inutilizacao.Modelo := Modelo;
            Inutilizacao.Serie := Serie;
            Inutilizacao.Ano := Ano;
            Inutilizacao.NumeroInicial := NumeroInicial;
            Inutilizacao.NumeroFinal := NumeroFinal;

            if Inutilizacao.Executar then
            begin
              Resposta.Msg := Inutilizacao.Msg;
              Resposta.Versao := Inutilizacao.versao;
              Resposta.TpAmb := TpAmbToStr(Inutilizacao.TpAmb);
              Resposta.VerAplic := Inutilizacao.VerAplic;
              Resposta.CStat := Inutilizacao.CStat;
              Resposta.XMotivo := Inutilizacao.XMotivo;
              Resposta.CUF := Inutilizacao.cUF;
              Resposta.DhRecbto := Inutilizacao.DhRecbto;
              Resposta.NomeArquivo := Inutilizacao.NomeArquivo;
              Resposta.Xml := Inutilizacao.XML_ProcInutNFe;

              MoverStringParaPChar(Resposta.Gerar, sResposta, esTamanho);
              Result := SetRetorno(ErrOK, StrPas(sResposta));
            end
            else
              Result := SetRetornoWebService(SSL.HTTPResultCode, 'InutilizarNFe');
          end;
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
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

initialization
  pLibClass := TACBrLibNFe;

end.
