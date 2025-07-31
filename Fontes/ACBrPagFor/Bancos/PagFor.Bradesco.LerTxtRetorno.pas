{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit PagFor.Bradesco.LerTxtRetorno;

interface

uses
  SysUtils, Classes,
  CNAB240.LerTxtRetorno, ACBrPagForClass;

type
 { TArquivoR_Bradesco }

  TArquivoR_Bradesco = class(TArquivoR_CNAB240)
  protected
    procedure LerRegistro1(nLinha: Integer); override;

    procedure LerSegmentoA(nLinha: Integer); override;

    procedure LerSegmentoJ(nLinha: Integer; var LeuRegistroJ: boolean); override;

    procedure LerSegmentoZ(mSegmentoZList: TSegmentoZList; nLinha: Integer); override;

    procedure LerSegmento5(mSegmento5List: TSegmento5List; nLinha: Integer); override;
  end;

implementation

uses
  ACBrPagForConversao;

{ TArquivoR_Bradesco }

procedure TArquivoR_Bradesco.LerRegistro1(nLinha: Integer);
var
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];

  PagFor.Lote.New;

  with PagFor.Lote.Last.Registro1.Servico do
  begin
    Operacao := StrToTpOperacao(mOk, LerCampo(Linha, 9, 1, tcStr));
    TipoServico := StrToTpServico(mOk, LerCampo(Linha, 10, 2, tcStr));
    FormaLancamento := StrToFmLancamento(mOk, LerCampo(Linha, 12, 2, tcStr));
  end;

  with PagFor.Lote.Last.Registro1.Empresa do
  begin
    Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));
    Inscricao.Numero := LerCampo(Linha, 19, 14, tcStr);
    Convenio := LerCampo(Linha, 33, 20, tcStr);

    ContaCorrente.Agencia.Codigo := LerCampo(Linha, 53, 5, tcInt);
    ContaCorrente.Agencia.DV := LerCampo(Linha, 58, 1, tcStr);
    ContaCorrente.Conta.Numero := LerCampo(Linha, 59, 12, tcInt64);
    ContaCorrente.Conta.DV := LerCampo(Linha, 71, 1, tcStr);
    ContaCorrente.DV := LerCampo(Linha, 72, 1, tcStr);

    Nome := LerCampo(Linha, 73, 30, tcStr);
  end;

  PagFor.Lote.Last.Registro1.Informacao1 := LerCampo(Linha, 103, 40, tcStr);

  case PagFor.Lote.Last.Registro1.Servico.TipoServico of
    tsConciliacaoBancaria:
      begin
        with PagFor.Lote.Last.Registro1 do
        begin
          Data := LerCampo(Linha, 143, 8, tcDat);
          Valor := LerCampo(Linha, 151, 18, tcDe2);
          Situacao := LerCampo(Linha, 169, 1, tcStr);
          Status := LerCampo(Linha, 170, 1, tcStr);
          TipoMoeda := LerCampo(Linha, 171, 3, tcStr);
          Sequencia := LerCampo(Linha, 174, 5, tcInt);
        end;
      end;
  else
    begin
      with PagFor.Lote.Last.Registro1.Endereco do
      begin
        Logradouro := LerCampo(Linha, 143, 30, tcStr);
        Numero := LerCampo(Linha, 173, 5, tcInt);
        Complemento := LerCampo(Linha, 178, 15, tcStr);
        Cidade := LerCampo(Linha, 193, 20, tcStr);
        CEP := LerCampo(Linha, 213, 8, tcInt);
        Estado := LerCampo(Linha, 221, 2, tcStr);
      end;
    end;
  end;

  with PagFor.Lote.Last.Registro1 do
  begin
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, '1', '', '');
  end;
end;

procedure TArquivoR_Bradesco.LerSegmentoA(nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
  x: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3A' then
    Exit;

  PagFor.Lote.Last.SegmentoA.New;

  with PagFor.Lote.Last.SegmentoA.Last do
  begin
    TipoMovimento := StrToTpMovimento(mOk, LerCampo(Linha, 15, 1, tcStr));
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
    Favorecido.Camara := LerCampo(Linha, 18, 3, tcInt);
    Favorecido.Banco := StrToBanco(mOk, LerCampo(Linha, 21, 3, tcStr));

    with Favorecido do
    begin
      ContaCorrente.Agencia.Codigo := LerCampo(Linha, 24, 5, tcInt);
      ContaCorrente.Agencia.DV := LerCampo(Linha, 29, 1, tcStr);
      ContaCorrente.Conta.Numero := LerCampo(Linha, 30, 12, tcInt64);
      ContaCorrente.Conta.DV := LerCampo(Linha, 42, 1, tcStr);
      ContaCorrente.DV := LerCampo(Linha, 43, 1, tcStr);
    end;

    Favorecido.Nome := LerCampo(Linha, 44, 20, tcStr);
    Credito.SeuNumero := LerCampo(Linha, 74, 20, tcStr);
    Credito.DataPagamento := LerCampo(Linha, 94, 8, tcDat);

    with Credito do
    begin
  //    Moeda.Tipo := StrToTpMoeda(mOk, LerCampo(Linha, 102, 3, tcStr));
  //    Moeda.Qtde := LerCampo(Linha, 105, 15, tcDe5);
      ValorPagamento := LerCampo(Linha, 120, 15, tcDe2);
      NossoNumero := LerCampo(Linha, 135, 20, tcStr);
      DataReal := LerCampo(Linha, 155, 8, tcDat);
      ValorReal := LerCampo(Linha, 163, 15, tcDe2);
    end;

    Informacao2 := LerCampo(Linha, 178, 40, tcStr);
    CodigoDOC := LerCampo(Linha, 218, 2, tcStr);
    CodigoTED := LerCampo(Linha, 220, 5, tcStr);
    Aviso := LerCampo(Linha, 230, 1, tcInt);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'A', '', Credito.SeuNumero);
  end;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  while Pos(RegSeg, '3B/3C/3D/3E/3F/3Z/') > 0 do
  begin
    Inc(nLinha); //pr�xima linha do txt a ser lida
    {opcionais do segmento A}
    LerSegmentoB(PagFor.Lote.Last.SegmentoA.Last.SegmentoB, nLinha);
    LerSegmentoC(PagFor.Lote.Last.SegmentoA.Last.SegmentoC, nLinha);
//    LerSegmentoE(PagFor.Lote.Last.SegmentoA.Last.SegmentoE, I);
//    LerSegmentoF(PagFor.Lote.Last.SegmentoA.Last.SegmentoF, I);
    LerSegmentoZ(PagFor.Lote.Last.SegmentoA.Last.SegmentoZ, nLinha);

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoB.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'B',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoC.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoC.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'C',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoZ.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoZ.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, 'A', 'Z',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  end;
end;

procedure TArquivoR_Bradesco.LerSegmentoJ(nLinha: Integer;
  var LeuRegistroJ: boolean);
var
  RegOpc: string;
begin
  RegOpc := LerCampo(Linha, 18, 2, tcStr);

  if RegOpc = '52' then
    Exit;

  inherited LerSegmentoJ(nLinha, LeuRegistroJ);
end;

procedure TArquivoR_Bradesco.LerSegmentoZ(mSegmentoZList: TSegmentoZList;
  nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3Z' then
    Exit;

  mSegmentoZList.New;

  with mSegmentoZList.Last do
  begin
    Autenticacao := LerCampo(Linha, 15, 64, tcStr);
    SeuNumero := LerCampo(Linha, 79, 25, tcStr);

    case PagFor.Lote[0].Registro1.Servico.FormaLancamento of
      flCreditoContaCorrente, flChequePagamento, flDocTed, flOPDisposicao,
      flPagamentoAutenticacao:
        begin
          ControleOBB := LerCampo(Linha, 104, 3, tcStr);
        end;

      flTributoDARFNormal, flTributoGPS, flTributoDARFSimples, flTributoIPTU,
      flTributoDARJ, flTributoGARESPICMS, flTributoGARESPDR, flTributoGARESPITCMD,
      flTributoIPVA, flTributoLicenciamento, flTributoDPVAT, flTributoGNRe:
        begin
          RefProduto := LerCampo(Linha, 153, 40, tcStr);
          DescProduto := LerCampo(Linha, 193, 23, tcStr);
          Documento := LerCampo(Linha, 216, 15, tcStr);
        end;
    end;

    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, 'Z', '', '');
  end;
end;

procedure TArquivoR_Bradesco.LerSegmento5(mSegmento5List: TSegmento5List; nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '35' then
    Exit;

  mSegmento5List.New;

  with mSegmento5List.Last do
  begin
    ListaDebito := LerCampo(Linha, 18, 9, tcStr);
    HorarioDebito := LerCampo(Linha, 27, 6, tcHor);
    CodLancamento := LerCampo(Linha, 33, 5, tcInt);
    SegLinhaExtrato := LerCampo(Linha, 38, 5, tcInt);
    UsoEmpresa := LerCampo(Linha, 43, 50, tcStr);
    TipoDocumento := LerCampo(Linha, 93, 3, tcInt);
    NumeroDocumento := LerCampo(Linha, 96, 15, tcStr);
    SerieDocumento := LerCampo(Linha, 111, 2, tcStr);
    DataEmissao := LerCampo(Linha, 128, 8, tcStr);

    case PagFor.Lote[0].Registro1.Servico.FormaLancamento of
      flCreditoContaCorrente, flChequePagamento, flDocTed, flOPDisposicao,
      flPagamentoAutenticacao:
        begin
          NomeReclamante := LerCampo(Linha, 136, 30, tcStr);
          NumeroProcesso := LerCampo(Linha, 166, 25, tcStr);
          PISPASEP := LerCampo(Linha, 191, 15, tcStr);
        end;

      flTributoDARFNormal, flTributoGPS, flTributoDARFSimples, flTributoIPTU,
      flTributoDARJ, flTributoGARESPICMS, flTributoGARESPDR, flTributoGARESPITCMD,
      flTributoIPVA, flTributoLicenciamento, flTributoDPVAT, flTributoGNRe:
        begin
          Versao := LerCampo(Linha, 136, 3, tcStr);
          HorarioEfetivacao := LerCampo(Linha, 139, 6, tcHor);
          CodReceita := LerCampo(Linha, 145, 6, tcStr);
          CodMunicipio := LerCampo(Linha, 151, 4, tcInt);
          Placa := LerCampo(Linha, 155, 7, tcStr);
          NumeroAIIM := LerCampo(Linha, 162, 9, tcStr);
          InscDividaAtiva := LerCampo(Linha, 171, 13, tcStr);
          Exercicio := LerCampo(Linha, 184, 4, tcInt);
          CotaUnica := LerCampo(Linha, 188, 10, tcStr);
        end;
    end;

    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

    GerarAvisos(CodOcorrencia, '5', '', '');
  end;
end;

end.

