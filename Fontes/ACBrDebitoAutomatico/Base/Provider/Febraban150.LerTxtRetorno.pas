{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit Febraban150.LerTxtRetorno;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrDebitoAutomaticoLerTxt, ACBrDebitoAutomaticoClass, ACBrDebitoAutomaticoConversao;

type
  { TArquivoR_Febraban150 }

  TArquivoR_Febraban150 = class(TArquivoRClass)
  private
    FArquivoTXT: TStringList;

  protected
    FLayoutVersao: TDebitoLayoutVersao;
    FnLinha: Integer;
    FLinha: string;

    procedure LimparRegistros;

    procedure LerRegistroA; virtual;
    procedure LerRegistroB; virtual;
    procedure LerRegistroC; virtual;
    procedure LerRegistroD; virtual;
    procedure LerRegistroE; virtual;
    procedure LerRegistroF; virtual;
    procedure LerRegistroH; virtual;
    procedure LerRegistroI; virtual;
    procedure LerRegistroJ; virtual;
    procedure LerRegistroK; virtual;
    procedure LerRegistroL; virtual;
    procedure LerRegistroT; virtual;
    procedure LerRegistroX; virtual;
    procedure LerRegistroZ; virtual;

    procedure GerarAvisos(const aCodOcorrencia: TDebitoRetorno; aDescOcorrencia,
      aSegmento, aIdentificacao: string);

    function GetRetorno(aOcorrencia: TDebitoRetorno): string; virtual;
  public
    function LerTxt: Boolean; override;

    function DescricaoRetorno(const ADesc: string): string; virtual;

    property ArquivoTXT: TStringList read FArquivoTXT write FArquivoTXT;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TArquivoR_Febraban150 }

procedure TArquivoR_Febraban150.GerarAvisos(const aCodOcorrencia: TDebitoRetorno;
  aDescOcorrencia, aSegmento, aIdentificacao: string);
begin
  if aCodOcorrencia <> trEsp then
  begin
    DebitoAutomatico.RegistroA.Aviso.New;

    with DebitoAutomatico.RegistroA.Aviso.Last do
    begin
      CodigoRetorno := RetornoToStr(aCodOcorrencia);
      MensagemRetorno := aDescOcorrencia;
      Segmento := aSegmento;
      Identificacao := aIdentificacao;
    end;
  end;
end;

function TArquivoR_Febraban150.GetRetorno(aOcorrencia: TDebitoRetorno): string;
begin
  case aOcorrencia of
    tr00: Result := 'Débito efetuado';
    tr01: Result := 'Débito não efetuado - Insuficiência de fundos';
    tr02: Result := 'Débito não efetuado - Conta não cadastrada';
    tr04: Result := 'Débito não efetuado - Outras restrições';
    tr05: Result := 'Débito não efetuado – valor do débito excede valor limite aprovado';
    tr10: Result := 'Débito não efetuado - Agência em regime de encerramento';
    tr12: Result := 'Débito não efetuado - Valor inválido';
    tr13: Result := 'Débito não efetuado - Data de lançamento inválida';
    tr14: Result := 'Débito não efetuado - Agência inválida';
    tr15: Result := 'Débito não efetuado - conta inválida';
    tr18: Result := 'Débito não efetuado - Data do débito anterior à do processamento';
    tr19: Result := 'Débito não efetuado – Agência/Conta não pertence ao CPF/CNPJ informado';
    tr20: Result := 'Débito não efetuado – conta conjunta não solidária';
    tr30: Result := 'Débito não efetuado - Sem contrato de débito automático';
    tr31: Result := 'Débito efetuado em data diferente da data informada – feriado na praça de débito';
    tr96: Result := 'Manutenção do Cadastro';
    tr97: Result := 'Cancelamento - Não encontrado';
    tr98: Result := 'Cancelamento - Não efetuado, fora do tempo hábil';
    tr99: Result := 'Cancelamento – cancelado conforme solicitação';
    trDP: Result := 'Débito efetuado parcialmente';
    trFP: Result := 'Prazo inferior a 10 dias de antecedência da data de vencimento';
    trCF: Result := 'Cadastramento – Cadastro realizado';
    trNC: Result := 'Cadastramento não realizado – Conta inválida';
    trCH: Result := 'Cadastramento não realizado – Opção de uso do cheque especial inválida';
    trPV: Result := 'Cadastramento não realizado – Opção de débito após o vencimento inválida';
    trDT: Result := 'Cadastramento não realizado – Data de vencimento do cadastramento inválida';
    trOP: Result := 'Cadastramento não realizado – Tipo de operação inválida';
    trCE: Result := 'Cadastramento não realizado – Cadastro já existente';
    trCD: Result := 'Cancelamento de cadastro realizado na depositária';
    trPB: Result := 'Débito não realizado – Cliente com Portabilidade';
  else
    Result := '';
  end;

  Result := ACBrStr(Result);
end;

function TArquivoR_Febraban150.DescricaoRetorno(const ADesc: string): string;
var
  xDesc, xOcorr: string;
  xOcorrencia: TDebitoRetorno;
  Ok: Boolean;
begin
  // O código de ocorrencia pode ter até 5 códigos de 2 dígitos cada
  xDesc := ADesc;

  while length(xDesc) > 0 do
  begin
    xOcorr := Copy(xDesc, 1, 2);
    xOcorrencia := StrToRetorno(Ok, xOcorr);

    if Ok then
      Result := Result + '/' + GetRetorno(xOcorrencia)
    else
      Result := Result + '/ (' + xOcorr + ') Retorno Nao Identificado';

    Delete(xDesc, 1, 2);
  end;

  if Result <> '' then
    Delete(Result, 1, 1);
end;

procedure TArquivoR_Febraban150.LerRegistroA;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'A' then
      break;

    with DebitoAutomatico.RegistroA do
    begin
      CodigoRemessa := LerCampo(FLinha, 2, 1, tcStr);
      CodigoConvenio := LerCampo(FLinha, 3, 20, tcStr);
      NomeEmpresa := LerCampo(FLinha, 23, 20, tcStr);
      CodigoBanco := LerCampo(FLinha, 43, 3, tcInt);
      NomeBanco := LerCampo(FLinha, 46, 20, tcStr);
      Geracao := LerCampo(FLinha, 66, 8, tcDatISO);
      NSA := LerCampo(FLinha, 76, 6, tcInt);
      LayoutVersao := StrToLayoutVersao(Ok, LerCampo(FLinha, 80, 2, tcStr));

      FLayoutVersao := LayoutVersao;
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroB;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'B' then
      break;

    with DebitoAutomatico.RegistroB.New do
    begin
      IdentificacaoClienteEmpresa := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);

      if FLayoutVersao = lv8 then
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 20, tcStr);
        DataOpcaoExclusao := LerCampo(FLinha, 51, 8, tcDatISO);
      end
      else
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
        DataOpcaoExclusao := LerCampo(FLinha, 45, 8, tcDatISO);
      end;

      CodigoMovimento := StrToMovimentoCadastro(Ok, LerCampo(FLinha, 150, 1, tcStr));
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroC;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'C' then
      break;

    with DebitoAutomatico.RegistroC.New do
    begin
      IdentificacaoClienteEmpresa := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);

      if FLayoutVersao = lv8 then
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 20, tcStr);
        Ocorrencia1 := LerCampo(FLinha, 51, 40, tcStr);
        Ocorrencia2 := LerCampo(FLinha, 91, 40, tcStr);
      end
      else
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
        Ocorrencia1 := LerCampo(FLinha, 45, 40, tcStr);
        Ocorrencia2 := LerCampo(FLinha, 85, 40, tcStr);
      end;

      CodigoMovimento := StrToMovimentoCadastro(Ok, LerCampo(FLinha, 150, 1, tcStr));
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroD;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'D' then
      break;

    with DebitoAutomatico.RegistroD.New do
    begin
      IdentificacaoClienteEmpresaAnterior := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);

      if FLayoutVersao = lv8 then
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 20, tcStr);
        IdentificacaoClienteEmpresaAtual := LerCampo(FLinha, 51, 25, tcStr);
        Ocorrencia := LerCampo(FLinha, 76, 55, tcStr);
        NovaDataVencAutorizacao := LerCampo(FLinha, 131, 8, tcDatISO);
        AlteracaoOpcaoUsoChequeEspecial := StrToAlteracaoOpcao(Ok, LerCampo(FLinha, 139, 1, tcStr));
        AlteracaoOpcaoDebParcialIntegralPosVenc := StrToAlteracaoOpcao(Ok, LerCampo(FLinha, 140, 1, tcStr));
      end
      else
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
        IdentificacaoClienteEmpresaAtual := LerCampo(FLinha, 51, 25, tcStr);
        Ocorrencia := LerCampo(FLinha, 70, 60, tcStr);
      end;

      CodigoMovimento := StrToMovimentoAlteracao(Ok, LerCampo(FLinha, 150, 1, tcStr));
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroE;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'E' then
      break;

    with DebitoAutomatico.RegistroE.New do
    begin
      IdentificacaoClienteEmpresa := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);

      if FLayoutVersao = lv8 then
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 20, tcStr);
        Vencimento := LerCampo(FLinha, 51, 8, tcDatISO);
        DebitoMoeda := StrToMoeda(Ok, LerCampo(FLinha, 74, 2, tcStr));

        if DebitoMoeda = mUFIR then
          Valor := LerCampo(FLinha, 59, 15, tcDe5)
        else
          Valor := LerCampo(FLinha, 59, 15, tcDe2);

        UsoEmpresa := LerCampo(FLinha, 70, 63, tcStr);
        UsoEmpresaXY := StrToUsoEmpresaXY(Ok, LerCampo(FLinha, 129, 1, tcStr));
        TipoIdentificacao := StrToCPFCNPJ(Ok, LerCampo(FLinha, 130, 1, tcStr));
        CPFCNPJ := LerCampo(FLinha, 131, 15, tcStr);
        TipoOperacao := StrToTipoOperacao(Ok, LerCampo(FLinha, 146, 1, tcStr));
        UtilizacaoChequeEspecial := StrToSimNao(Ok, LerCampo(FLinha, 147, 1, tcStr));
        OpcaoDebParcialIntegralPosVenc := StrToSimNao(Ok, LerCampo(FLinha, 148, 1, tcStr));
      end
      else
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
        Vencimento := LerCampo(FLinha, 45, 8, tcDatISO);
        DebitoMoeda := StrToMoeda(Ok, LerCampo(FLinha, 68, 2, tcStr));

        if DebitoMoeda = mUFIR then
          Valor := LerCampo(FLinha, 53, 15, tcDe5)
        else
          Valor := LerCampo(FLinha, 53, 15, tcDe2);

        UsoEmpresaXY := StrToUsoEmpresaXY(Ok, LerCampo(FLinha, 129, 1, tcStr));

        if FLayoutVersao = lv5 then
        begin
          UsoEmpresa := LerCampo(FLinha, 70, 59, tcStr);
          TipoIdentificacao := StrToCPFCNPJ(Ok, LerCampo(FLinha, 130, 1, tcStr));
          CPFCNPJ := LerCampo(FLinha, 131, 15, tcStr);
        end
        else
        begin
          UsoEmpresa := LerCampo(FLinha, 70, 49, tcStr);
          UsoEmpresaValorTotalTributos := LerCampo(FLinha, 119, 10, tcDe2);
        end;
      end;

      CodigoMovimento := StrToMovimentoDebito(Ok, LerCampo(FLinha, 150, 1, tcStr));
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroF;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'F' then
      break;

    with DebitoAutomatico.RegistroF.New do
    begin
      IdentificacaoClienteEmpresa := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);

      if FLayoutVersao = lv8 then
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 20, tcStr);
        Vencimento := LerCampo(FLinha, 51, 8, tcDatISO);
        Valor := LerCampo(FLinha, 59, 15, tcDe2);
        CodigoRetorno := StrToRetorno(Ok, LerCampo(FLinha, 74, 2, tcStr));
      end
      else
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
        Vencimento := LerCampo(FLinha, 45, 8, tcDatISO);
        Valor := LerCampo(FLinha, 53, 15, tcDe2);
        CodigoRetorno := StrToRetorno(Ok, LerCampo(FLinha, 68, 2, tcStr));
      end;

      case FLayoutVersao of
        lv5,
        lv6:
          begin
            UsoEmpresa := LerCampo(FLinha, 70, 60, tcStr);
            TipoIdentificacao := StrToCPFCNPJ(Ok, LerCampo(FLinha, 130, 1, tcStr));
            CPFCNPJ := LerCampo(FLinha, 131, 15, tcStr);
          end;
        lv8:
          begin
            UsoEmpresa := LerCampo(FLinha, 76, 54, tcStr);
            TipoIdentificacao := StrToCPFCNPJ(Ok, LerCampo(FLinha, 130, 1, tcStr));
            CPFCNPJ := LerCampo(FLinha, 131, 15, tcStr);
          end
      else
        UsoEmpresa := LerCampo(FLinha, 70, 70, tcStr);
      end;

      CodigoMovimento := StrToMovimentoDebito(Ok, LerCampo(FLinha, 150, 1, tcStr));

      DescRetorno := DescricaoRetorno(RetornoToStr(CodigoRetorno));
      GerarAvisos(CodigoRetorno, DescRetorno, 'F', IdentificacaoClienteBanco);
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroH;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'H' then
      break;

    with DebitoAutomatico.RegistroH.New do
    begin
      IdentificacaoClienteEmpresaAnterior := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);

      if FLayoutVersao = lv8 then
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 20, tcStr);
        IdentificacaoClienteEmpresaAtual := LerCampo(FLinha, 51, 25, tcStr);
        Ocorrencia := LerCampo(FLinha, 76, 52, tcStr);
        OcorrenciaCancelamento := StrToRetorno(Ok, LerCampo(FLinha, 128, 2, tcStr));
        OcorrenciaDataVigencia := StrToRetorno(Ok, LerCampo(FLinha, 130, 2, tcStr));
        OcorrenciaAlteracaoOpcaoUsoChequeEspecial := StrToRetorno(Ok, LerCampo(FLinha, 132, 2, tcStr));
        OcorrenciaAlteracaoOpcaoDebPosVenc := StrToRetorno(Ok, LerCampo(FLinha, 142, 2, tcStr));

        DescRetornoCanc := DescricaoRetorno(RetornoToStr(OcorrenciaCancelamento));
        GerarAvisos(OcorrenciaCancelamento, DescRetornoCanc, 'H', IdentificacaoClienteBanco);
        DescRetornoDatVig := DescricaoRetorno(RetornoToStr(OcorrenciaDataVigencia));
        GerarAvisos(OcorrenciaDataVigencia, DescRetornoDatVig, 'H', IdentificacaoClienteBanco);
        DescRetornoChequeEsp := DescricaoRetorno(RetornoToStr(OcorrenciaAlteracaoOpcaoUsoChequeEspecial));
        GerarAvisos(OcorrenciaAlteracaoOpcaoUsoChequeEspecial, DescRetornoChequeEsp, 'H', IdentificacaoClienteBanco);
        DescRetornoPosVenc := DescricaoRetorno(RetornoToStr(OcorrenciaAlteracaoOpcaoDebPosVenc));
        GerarAvisos(OcorrenciaAlteracaoOpcaoDebPosVenc, DescRetornoPosVenc, 'H', IdentificacaoClienteBanco);
      end
      else
      begin
        IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
        IdentificacaoClienteEmpresaAtual := LerCampo(FLinha, 45, 25, tcStr);
        Ocorrencia := LerCampo(FLinha, 70, 58, tcStr);
      end;

      CodigoMovimento := StrToMovimentoAlteracao(Ok, LerCampo(FLinha, 150, 1, tcStr));
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroI;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'I' then
      break;

    with DebitoAutomatico.RegistroI.New do
    begin
      IdentificacaoClienteEmpresa := LerCampo(FLinha, 2, 25, tcStr);
      TipoIdentificacao := StrToCPFCNPJ(Ok, LerCampo(FLinha, 27, 1, tcStr));
      CPFCNPJ := LerCampo(FLinha, 28, 14, tcStr);
      Nome := LerCampo(FLinha, 42, 40, tcStr);
      Cidade := LerCampo(FLinha, 82, 30, tcStr);
      UF := LerCampo(FLinha, 112, 2, tcStr);
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroJ;
var
  TipoReg: string;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'J' then
      break;

    with DebitoAutomatico.RegistroJ.New do
    begin
      NSA := LerCampo(FLinha, 2, 6, tcInt);
      Geracao := LerCampo(FLinha, 8, 8, tcDatISO);
      TotalRegistros := LerCampo(FLinha, 16, 6, tcInt);
      ValorTotal := LerCampo(FLinha, 22, 17, tcDe2);
      Processamento := LerCampo(FLinha, 39, 8, tcDatISO);
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroK;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'K' then
      break;

    with DebitoAutomatico.RegistroK.New do
    begin
      IdentificacaoClienteEmpresa := LerCampo(FLinha, 2, 25, tcStr);
      AgenciaDebito := LerCampo(FLinha, 27, 4, tcStr);
      IdentificacaoClienteBanco := LerCampo(FLinha, 31, 14, tcStr);
      TipoTratamento := LerCampo(FLinha, 45, 2, tcInt);
      Valor := LerCampo(FLinha, 47, 15, tcDe2);
      CodigoReceita := LerCampo(FLinha, 62, 4, tcInt);
      TipoIdentificacao := StrToCPFCNPJ(Ok, LerCampo(FLinha, 66, 1, tcStr));
      CPFCNPJ := LerCampo(FLinha, 67, 15, tcStr);
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroL;
var
  TipoReg: string;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'L' then
      break;

    with DebitoAutomatico.RegistroL.New do
    begin
      FaturamentoContas := LerCampo(FLinha, 2, 8, tcDatISO);
      VencimentoFatura := LerCampo(FLinha, 10, 8, tcDatISO);
      RemessaArquivoBanco := LerCampo(FLinha, 18, 8, tcDatISO);
      RemessaContasFisicas := LerCampo(FLinha, 26, 8, tcDatISO);
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroT;
var
  TipoReg: string;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'T' then
      break;

    with DebitoAutomatico.RegistroT.New do
    begin
      Total := LerCampo(FLinha, 2, 6, tcInt);
      ValorTotal := LerCampo(FLinha, 8, 17, tcDe2);
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroX;
var
  TipoReg: string;
  Ok: Boolean;
begin
  while true do
  begin
    FLinha := ArquivoTXT.Strings[FnLinha];
    TipoReg := LerCampo(FLinha, 1, 1, tcStr);

    if TipoReg <> 'X' then
      break;

    with DebitoAutomatico.RegistroX.New do
    begin
      CodigoAgencia := LerCampo(FLinha, 2, 4, tcStr);
      NomeAgencia := LerCampo(FLinha, 6, 30, tcStr);
      EnderecoAgencia := LerCampo(FLinha, 36, 30, tcStr);
      Numero := LerCampo(FLinha, 66, 5, tcStr);
      CEP := LerCampo(FLinha, 71, 8, tcStr);
      NomeCidade := LerCampo(FLinha, 79, 20, tcStr);
      UF := LerCampo(FLinha, 99, 2, tcStr);
      SituacaoAgencia := StrToSituacaoAgencia(Ok, LerCampo(FLinha, 101, 1, tcStr));
    end;

    Inc(FnLinha);
  end;
end;

procedure TArquivoR_Febraban150.LerRegistroZ;
var
  TipoReg: string;
begin
  FLinha := ArquivoTXT.Strings[FnLinha];
  TipoReg := LerCampo(FLinha, 1, 1, tcStr);

  if TipoReg <> 'Z' then
    Exit;

  with DebitoAutomatico.RegistroZ do
  begin
    Total := LerCampo(FLinha, 2, 6, tcInt);
    ValorTotal := LerCampo(FLinha, 8, 17, tcDe2);
  end;
end;

function TArquivoR_Febraban150.LerTxt: Boolean;
begin
  {
  RETORNO: enviado pela Instituição Depositária (banco),
           para a Instituição Destinatária (empresa).
          Este arquivo poderá conter os seguintes registros:
          A, B, C, D, E, F, H, I, J, K, L, T, X e Z.
  }
  ArquivoTXT := TStringList.Create;

  try
    try
      ArquivoTXT.Text := Arquivo;

      FnLinha := 1;
      LimparRegistros;

      LerRegistroA;
      LerRegistroB;
      LerRegistroC;
      LerRegistroD;
      LerRegistroE;
      LerRegistroF;
      LerRegistroH;
      LerRegistroI;
      LerRegistroJ;
      LerRegistroK;
      LerRegistroL;
      LerRegistroT;
      LerRegistroX;
      LerRegistroZ;

      Result := True;
    except
      on E: Exception do
      begin
        raise Exception.Create('Não Foi Possível ler os Registros do Arquivo' + #13 +
          E.Message);
      end;
    end;
  finally
    ArquivoTXT.Free;
  end;
end;

procedure TArquivoR_Febraban150.LimparRegistros;
begin
  with DebitoAutomatico do
  begin
    RegistroA.CodigoConvenio := '';
    RegistroA.NomeEmpresa := '';
    RegistroA.CodigoBanco := 0;
    RegistroA.NomeBanco := '';
    RegistroA.Geracao := Now;
    RegistroA.NSA := 0;
    RegistroA.LayoutVersao := lv4;

    RegistroB.Clear;
    RegistroC.Clear;
    RegistroD.Clear;
    RegistroE.Clear;
    RegistroF.Clear;
    RegistroH.Clear;
    RegistroI.Clear;
    RegistroJ.Clear;
    RegistroK.Clear;
    RegistroL.Clear;
    RegistroT.Clear;
    RegistroX.Clear;

    RegistroZ.Total := 0;
    RegistroZ.ValorTotal := 0;
  end;
end;

end.
