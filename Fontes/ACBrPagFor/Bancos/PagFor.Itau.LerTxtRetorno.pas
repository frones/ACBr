{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit PagFor.Itau.LerTxtRetorno;

interface

uses
  SysUtils, Classes,
  CNAB240.LerTxtRetorno, ACBrPagForClass;

type
 { TArquivoR_Itau }

  TArquivoR_Itau = class(TArquivoR_CNAB240)
  protected
    procedure LerRegistro5(nLinha: Integer); override;

    procedure LerSegmentoA(nLinha: Integer); override;

    procedure LerSegmentoB(mSegmentoBList: TSegmentoBList; nLinha: Integer); override;

    procedure LerSegmentoC(mSegmentoCList: TSegmentoCList; nLinha: Integer); override;

    procedure LerSegmentoN1(nLinha: Integer); override;

    procedure LerSegmentoN2(nLinha: Integer); override;

    procedure LerSegmentoN3(nLinha: Integer); override;

    procedure LerSegmentoN4(nLinha: Integer); override;

    procedure LerSegmentoN567(nLinha: Integer); override;

    procedure LerSegmentoN8(nLinha: Integer); override;

    procedure LerSegmentoN9(nLinha: Integer); override;

    procedure LerSegmentoO(nLinha: Integer); override;
  end;

implementation

uses
  ACBrUtil.DateTime,
  ACBrPagForConversao;

{ TArquivoR_Itau }

procedure TArquivoR_Itau.LerRegistro5(nLinha: Integer);
var
  xAviso: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];

  case PagFor.Lote.Last.Registro1.Servico.TipoServico of
    tsConciliacaoBancaria:
      begin
        with PagFor.Lote.Last.Registro5 do
        begin
          BloqueadoAcima24h := LerCampo(Linha, 89, 18, tcDe2);
          Limite := LerCampo(Linha, 107, 18, tcDe2);
          BloqueadoAte24h := LerCampo(Linha, 125, 18, tcDe2);
          Data := LerCampo(Linha, 143, 8, tcDat);
          Valor := LerCampo(Linha, 151, 18, tcDe2);
          Situacao := LerCampo(Linha, 169, 1, tcStr);
          Status := LerCampo(Linha, 170, 1, tcStr);
          QtdeRegistros := LerCampo(Linha, 171, 6, tcInt);
          ValorDebitos := LerCampo(Linha, 177, 18, tcDe2);
          ValorCreditos := LerCampo(Linha, 195, 18, tcDe2);
        end;
      end
  else
    begin
      with PagFor.Lote.Last.Registro5 do
      begin
        xAviso := LerCampo(Linha, 60, 1, tcStr);

        if PagFor.Lote.Last.Registro1.Servico.FormaLancamento = flPagamentoConcessionarias then
        begin
          // Contas de Concessionárias e Tributos com código de barras
          Valor := LerCampo(Linha, 24, 18, tcDe2);
          QtdeMoeda := LerCampo(Linha, 42, 15, tcDe5);
        end
        else
        begin
          if (PagFor.Lote.Last.Registro1.Servico.TipoServico = tsPagamentoSalarios) or
             (xAviso <> '') then
          begin
            // fgts
            Valor := LerCampo(Linha, 24, 14, tcDe2);
            TotalOutrasEntidades := LerCampo(Linha, 38, 14, tcDe2);
            TotalValorAcrescimo := LerCampo(Linha, 52, 14, tcDe2);
            TotalValorArrecadado := LerCampo(Linha, 66, 14, tcDe2);
          end
          else
          begin
            // Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
            // Liquidação de títulos (bloquetos) em cobrança no Itaú e em outros Bancos
            Valor := LerCampo(Linha, 24, 18, tcDe2);
          end;
        end;
      end;
    end;
  end;

  with PagFor.Lote.Last.Registro5 do
  begin
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
    DescOcorrencia := DescricaoRetorno(CodOcorrencia);

    GerarAvisos(CodOcorrencia, DescOcorrencia, '5', '', '');
  end;
end;

procedure TArquivoR_Itau.LerSegmentoA(nLinha: Integer);
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

    Informacao2 := LerCampo(Linha, 178, 20, tcStr);
    NumeroDocumento := LerCampo(Linha, 198, 6, tcInt);

    Favorecido.Inscricao.Numero := LerCampo(Linha, 204, 14, tcStr);

    CodigoDOC := LerCampo(Linha, 218, 2, tcStr);
    CodigoTED := LerCampo(Linha, 220, 5, tcStr);
    Aviso := LerCampo(Linha, 230, 1, tcInt);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
    DescOcorrencia := DescricaoRetorno(CodOcorrencia);

    GerarAvisos(CodOcorrencia, DescOcorrencia, 'A', '', Credito.SeuNumero);
  end;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  while Pos(RegSeg, '3B/3C/3D/3E/3F/3Z/') > 0 do
  begin
    Inc(nLinha); //próxima linha do txt a ser lida
    {opcionais do segmento A}
    LerSegmentoB(PagFor.Lote.Last.SegmentoA.Last.SegmentoB, nLinha);
    LerSegmentoC(PagFor.Lote.Last.SegmentoA.Last.SegmentoC, nLinha);
//    LerSegmentoE(PagFor.Lote.Last.SegmentoA.Last.SegmentoE, I);
//    LerSegmentoF(PagFor.Lote.Last.SegmentoA.Last.SegmentoF, I);
//    LerSegmentoZ(PagFor.Lote.Last.SegmentoA.Last.SegmentoZ, I);

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoB.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'A', 'B',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoC.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoC.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'A', 'C',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  end;
end;

procedure TArquivoR_Itau.LerSegmentoB(mSegmentoBList: TSegmentoBList;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3B') then
    Exit;

  if LerCampo(Linha, 15, 2, tcStr) <> '' then
  begin
    // PIX
    // O banco não implementou nada
  end
  else
  begin
    mSegmentoBList.New;

    with mSegmentoBList.Last do
    begin
      Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));
      Inscricao.Numero := LerCampo(Linha, 19, 14, tcStr);

      Endereco.Logradouro := LerCampo(Linha, 33, 30, tcStr);
      Endereco.Numero := LerCampo(Linha, 63, 5, tcStr);
      Endereco.Complemento := LerCampo(Linha, 68, 15, tcStr);
      Endereco.Bairro := LerCampo(Linha, 83, 15, tcStr);
      Endereco.Cidade := LerCampo(Linha, 98, 20, tcStr);
      Endereco.CEP := LerCampo(Linha, 118, 8, tcInt);
      Endereco.Estado := LerCampo(Linha, 126, 2, tcStr);

      Email := LerCampo(Linha, 128, 100, tcStr);
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoC(mSegmentoCList: TSegmentoCList;
  nLinha: Integer);
var
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3C') then
    Exit;

  mSegmentoCList.New;

  with mSegmentoCList.Last do
  begin
    ValorCSLL := LerCampo(Linha, 15, 15, tcDe2);
    Vencimento := LerCampo(Linha, 38, 8, tcDat);
    ValorDocumento := LerCampo(Linha, 46, 15, tcDe2);
    ValorPIS := LerCampo(Linha, 61, 15, tcDe2);
    ValorIR := LerCampo(Linha, 76, 15, tcDe2);
    ValorISS := LerCampo(Linha, 91, 15, tcDe2);
    ValorCOFINS := LerCampo(Linha, 106, 15, tcDe2);
    Descontos := LerCampo(Linha, 121, 15, tcDe2);
    Abatimentos := LerCampo(Linha, 136, 15, tcDe2);
    Deducoes := LerCampo(Linha, 151, 15, tcDe2);
    Mora := LerCampo(Linha, 166, 15, tcDe2);
    Multa := LerCampo(Linha, 181, 15, tcDe2);
    Acrescimos := LerCampo(Linha, 196, 15, tcDe2);

    NumeroFaturaDocumento := LerCampo(Linha, 211, 20, tcStr);
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN1(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  // Só processa se for GPS
  if (LerCampo(Linha, 18, 2, tcStr) <> '01') then
    Exit;

  PagFor.Lote.Last.SegmentoN1.New;

  with PagFor.Lote.Last.SegmentoN1.Last do
  begin
    CodigoPagamento := StrToCodigoPagamentoGps(mOk, LerCampo(Linha, 20, 4, tcStr));
    MesAnoCompetencia := LerCampo(Linha, 24, 6, tcInt);
    idContribuinte := LerCampo(Linha, 30, 14, tcStr);
    ValorTributo := LerCampo(Linha, 44, 14, tcDe2);
    ValorOutrasEntidades := LerCampo(Linha, 58, 14, tcDe2);
    AtualizacaoMonetaria := LerCampo(Linha, 72, 14, tcDe2);

    with SegmentoN do
    begin
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 86, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 100, 8, tcDat);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      NossoNumero := LerCampo(Linha, 216, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}
  with PagFor.Lote.Last.SegmentoN1.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN2(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  // Só processa se for DARF
  if (LerCampo(Linha, 18, 2, tcStr) <> '02') then
    Exit;

  PagFor.Lote.Last.SegmentoN2.New;

  with PagFor.Lote.Last.SegmentoN2.Last do
  begin
    Receita := LerCampo(Linha, 20, 4, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 24, 1, tcStr));
    idContribuinte := LerCampo(Linha, 25, 14, tcStr);
    Periodo := LerCampo(Linha, 39, 8, tcDat);
    Referencia := LerCampo(Linha, 47, 17, tcStr);
    ValorPrincipal := LerCampo(Linha, 64, 14, tcDe2);
    Multa := LerCampo(Linha, 78, 14, tcDe2);
    Juros := LerCampo(Linha, 92, 14, tcDe2);
    DataVencimento := LerCampo(Linha, 120, 8, tcDat);

    with SegmentoN do
    begin
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 106, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 128, 8, tcDat);
      NomeContribuinte := LerCampo(Linha, 166, 30, tcStr);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}
  with PagFor.Lote.Last.SegmentoN2.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN3(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  // Só processa se for DARF Simples
  if (LerCampo(Linha, 18, 2, tcStr) <> '03') then
    Exit;

  PagFor.Lote.Last.SegmentoN3.New;

  with PagFor.Lote.Last.SegmentoN3.Last do
  begin
    // Verificar as posições e os campos.
    Receita := LerCampo(Linha, 20, 4, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 24, 1, tcStr));
    idContribuinte := LerCampo(Linha, 25, 14, tcStr);
    Periodo := LerCampo(Linha, 39, 8, tcDat);
    ReceitaBruta := LerCampo(Linha, 64, 14, tcDe2);
    Percentual := LerCampo(Linha, 64, 14, tcDe2);
    ValorPrincipal := LerCampo(Linha, 64, 14, tcDe2);
    Multa := LerCampo(Linha, 78, 14, tcDe2);
    Juros := LerCampo(Linha, 92, 14, tcDe2);
    DataVencimento := LerCampo(Linha, 120, 8, tcDat);

    with SegmentoN do
    begin
      // Verificar as posições e os campos.
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 106, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 128, 8, tcDat);
      NomeContribuinte := LerCampo(Linha, 166, 30, tcStr);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN3.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN4(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  // Só processa se for GARE SP ICMS
  if (LerCampo(Linha, 18, 2, tcStr) <> '05') then
    Exit;

  PagFor.Lote.Last.SegmentoN4.New;

  with PagFor.Lote.Last.SegmentoN4.Last do
  begin
    // Verificar as posições e os campos.
    Receita := LerCampo(Linha, 20, 4, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 24, 1, tcStr));
    idContribuinte := LerCampo(Linha, 25, 14, tcStr);
    InscEst := LerCampo(Linha, 39, 12, tcStr);
    NumEtiqueta := LerCampo(Linha, 51, 13, tcStr);
    Referencia := LerCampo(Linha, 64, 6, tcInt);
    NumParcela := LerCampo(Linha, 70, 13, tcStr);
    ValorReceita := LerCampo(Linha, 83, 14, tcDe2);
    Juros := LerCampo(Linha, 97, 14, tcDe2);
    Multa := LerCampo(Linha, 111, 14, tcDe2);
    DataVencimento := LerCampo(Linha, 139, 8, tcDat);

    with SegmentoN do
    begin
      // Verificar as posições e os campos.
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 125, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 147, 8, tcDat);
      NomeContribuinte := LerCampo(Linha, 166, 30, tcStr);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      NossoNumero := LerCampo(Linha, 216, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN4.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN567(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  // Só processa se for IPVA/DPVAT
  if (Pos(LerCampo(Linha, 18, 2, tcStr), '07 08') = 0) then
    Exit;

  PagFor.Lote.Last.SegmentoN567.New;

  with PagFor.Lote.Last.SegmentoN567.Last do
  begin
    // Verificar as posições e os campos.
    Receita := LerCampo(Linha, 111, 6, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Tributo := StrToIndTributo(mOk, LerCampo(Linha, 133, 2, tcStr));
    Exercicio := LerCampo(Linha, 135, 4, tcInt);
    Renavam := LerCampo(Linha, 139, 9, tcStr);
    Estado := LerCampo(Linha, 148, 2, tcStr);
    Municipio := LerCampo(Linha, 150, 5, tcInt);
    Placa := LerCampo(Linha, 155, 7, tcStr);
    OpcaoPagamento := LerCampo(Linha, 162, 1, tcStr);
    NovoRenavam := LerCampo(Linha, 163, 12, tcStr);

    with SegmentoN do
    begin
      // Verificar as posições e os campos.
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 125, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 147, 8, tcDat);
      NomeContribuinte := LerCampo(Linha, 166, 30, tcStr);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      NossoNumero := LerCampo(Linha, 216, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN567.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN8(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  PagFor.Lote.Last.SegmentoN8.New;

  with PagFor.Lote.Last.SegmentoN8.Last do
  begin
    // Verificar as posições e os campos.
    Receita := LerCampo(Linha, 20, 4, tcInt);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 24, 1, tcStr));
    idContribuinte := LerCampo(Linha, 25, 14, tcStr);
    InscEst := LerCampo(Linha, 39, 12, tcStr);
    Origem := LerCampo(Linha, 51, 13, tcStr);
    ValorPrincipal := LerCampo(Linha, 83, 14, tcDe2);
    AtualizacaoMonetaria := LerCampo(Linha, 97, 14, tcDe2);
    Mora := LerCampo(Linha, 97, 14, tcDe2);
    Multa := LerCampo(Linha, 111, 14, tcDe2);
    DataVencimento := LerCampo(Linha, 139, 8, tcDat);
    PeriodoParcela := LerCampo(Linha, 20, 6, tcInt);

    with SegmentoN do
    begin
      // Verificar as posições e os campos.
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 125, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 147, 8, tcDat);
      NomeContribuinte := LerCampo(Linha, 166, 30, tcStr);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      NossoNumero := LerCampo(Linha, 216, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}

  with PagFor.Lote.Last.SegmentoN8.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoN9(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  // Só processa se for FGTS
  if (LerCampo(Linha, 18, 2, tcStr) <> '11') then
    Exit;

  PagFor.Lote.Last.SegmentoN9.New;

  with PagFor.Lote.Last.SegmentoN9.Last do
  begin
    // Verificar as posições e os campos.
    Receita := LerCampo(Linha, 20, 4, tcInt);

    if LerCampo(Linha, 24, 1, tcStr) = '1' then // Nesse segmento, 1 = CNPJ e 2 = CEI
      TipoContribuinte := tiCNPJ
    else
      TipoContribuinte := tiCPF;

    idContribuinte := LerCampo(Linha, 25, 14, tcStr);
    CodigoBarras := LerCampo(Linha, 39, 12, tcStr);
    Identificador := LerCampo(Linha, 51, 13, tcInt);
    Lacre := LerCampo(Linha, 83, 14, tcInt);
    LacreDigito := LerCampo(Linha, 97, 14, tcInt);

    with SegmentoN do
    begin
      // Verificar as posições e os campos.
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      ValorPagamento := LerCampo(Linha, 125, 14, tcDe2);
      DataPagamento := LerCampo(Linha, 147, 8, tcDat);
      NomeContribuinte := LerCampo(Linha, 166, 30, tcStr);
      SeuNumero := LerCampo(Linha, 196, 20, tcStr);
      NossoNumero := LerCampo(Linha, 216, 20, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
      DescOcorrencia := DescricaoRetorno(CodOcorrencia);

      GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', '', SeuNumero);
    end;
  end;

  {Adicionais segmento N}
  with PagFor.Lote.Last.SegmentoN9.Last.SegmentoN do
  begin
    LerSegmentoB(SegmentoB, nLinha);
    LerSegmentoW(SegmentoW, nLinha);
    LerSegmentoZ(SegmentoZ, nLinha);

    for x := 0 to SegmentoB.Count - 1 do
    begin
      with SegmentoB.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'N', 'B', SeuNumero);
      end;
    end;
  end;
end;

procedure TArquivoR_Itau.LerSegmentoO(nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3O' then
    Exit;

  PagFor.Lote.Last.SegmentoO.New;

  with PagFor.Lote.Last.SegmentoO.Last do
  begin
    CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
    CodigoBarras := LerCampo(Linha, 18, 48, tcStr);
    NomeConcessionaria := LerCampo(Linha, 66, 30, tcStr);
    DataVencimento := LerCampo(Linha, 96, 8, tcDat);
    QuantidadeMoeda := LerCampo(Linha, 107, 15, tcDe5);
    ValorPagamento := LerCampo(Linha, 122, 15, tcDe2);
    DataPagamento := LerCampo(Linha, 137, 8, tcDat);
    ValorPago := LerCampo(Linha, 145, 15, tcDe2);
    NotaFiscal := LerCampo(Linha, 163, 9, tcInt);
    SeuNumero := LerCampo(Linha, 175, 20, tcStr);
    NossoNumero := LerCampo(Linha, 216, 15, tcStr);
    CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);
    DescOcorrencia := DescricaoRetorno(CodOcorrencia);
  end;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  while Pos(RegSeg, '3Z') > 0 do
  begin
    Inc(nLinha); //próxima linha do txt a ser lida

    {opcionais segmento O}
    LerSegmentoW(PagFor.Lote.Last.SegmentoO.Last.SegmentoW, nLinha);
    LerSegmentoB(PagFor.Lote.Last.SegmentoO.Last.SegmentoB, nLinha);
    LerSegmentoZ(PagFor.Lote.Last.SegmentoO.Last.SegmentoZ, nLinha);

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  end;
end;

end.

