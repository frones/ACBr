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
  SysUtils, Classes, ACBrPagForConversao,
  CNAB240.LerTxtRetorno, ACBrPagForClass;

type
 { TArquivoR_Itau }

  TArquivoR_Itau = class(TArquivoR_CNAB240)
  protected
    procedure LerRegistro5(nLinha: Integer); override;

    procedure LerSegmentoA(nLinha: Integer); override;

    procedure LerSegmentoB(mSegmentoBList: TSegmentoBList; nLinha: Integer); override;

    procedure LerSegmentoC(mSegmentoCList: TSegmentoCList; nLinha: Integer); override;

    procedure LerSegmentoJ(nLinha: Integer; var LeuRegistroJ: boolean); override;

    procedure LerSegmentoN1(nLinha: Integer); override;

    procedure LerSegmentoN2(nLinha: Integer); override;

    procedure LerSegmentoN3(nLinha: Integer); override;

    procedure LerSegmentoN4(nLinha: Integer); override;

    procedure LerSegmentoN567(nLinha: Integer); override;

    procedure LerSegmentoN8(nLinha: Integer); override;

    procedure LerSegmentoN9(nLinha: Integer); override;

    procedure LerSegmentoO(nLinha: Integer); override;

    function GetOcorrencia(aOcorrencia: TOcorrencia): String; override;
  end;

implementation

uses
  ACBrUtil.DateTime;

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

    GerarAvisos(CodOcorrencia, '5', '', '');
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
      NossoNumero := LerCampo(Linha, 135, 15, tcStr);
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

    GerarAvisos(CodOcorrencia, 'A', '', Credito.SeuNumero);
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

procedure TArquivoR_Itau.LerSegmentoJ(nLinha: Integer; var LeuRegistroJ: boolean);
var
  mOk: Boolean;
  RegSeg, RegOpc: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  RegOpc := LerCampo(Linha, 18, 2, tcStr);

  if (RegSeg <> '3J') and (RegSeg <> '3B') and (RegSeg <> '3C') and (RegSeg <> '3Z') then
    Exit;

  if (RegSeg = '3J') then
    LeuRegistroJ := True;

  if (RegOpc <> '52') and (RegOpc <> '99') and
     (RegSeg <> '3B') and (RegSeg <> '3C') and (RegSeg <> '3Z') then
  begin
    PagFor.Lote.Last.SegmentoJ.New;

    with PagFor.Lote.Last.SegmentoJ.Last do
    begin
      CodMovimento := StrToInMovimento(mOk, LerCampo(Linha, 16, 2, tcStr));
      CodigoBarras := LerCampo(Linha, 18, 44, tcStr);
      NomeCedente := LerCampo(Linha, 62, 30, tcStr);
      DataVencimento := LerCampo(Linha, 92, 8, tcDat);

      ValorTitulo := LerCampo(Linha, 100, 15, tcDe2);
      Desconto := LerCampo(Linha, 115, 15, tcDe2);
      Acrescimo := LerCampo(Linha, 130, 15, tcDe2);
      DataPagamento := LerCampo(Linha, 145, 8, tcDat);
      ValorPagamento := LerCampo(Linha, 153, 15, tcDe2);
      QtdeMoeda := LerCampo(Linha, 168, 15, tcDe5);
      ReferenciaSacado := LerCampo(Linha, 183, 20, tcStr);
      NossoNumero := LerCampo(Linha, 216, 15, tcStr);
      CodOcorrencia := LerCampo(Linha, 231, 10, tcStr);

      GerarAvisos(CodOcorrencia, 'J', '', ReferenciaSacado);
    end;
  end;

  // Segmentos B, C, Z, etc. também existem para outros tipos de segmento que
  // não sejam o J, portanto, só deve processar nessa rotina se o lote que está
  // sendo processado é realmente de tipos J.
  // O Itau, por exemplo, retorna arquivo com segmentos A contendo segmentos B
  // quando é pagamento de PIX e nesse caso, não pode processar o segmento B
  // nessa rotina pois não se refere a segmentos J.

  if not LeuRegistroJ then
    exit;

  Linha := ArquivoTXT.Strings[nLinha+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  RegOpc := LerCampo(Linha, 18, 2, tcStr);

  while (Pos(RegSeg, '3B/3C/3D/3E/3F/3Z/') > 0) or
        (RegSeg = '3J') and (Pos(RegOpc, '52/99/') > 0) do
  begin
    Inc(nLinha); //próxima linha do txt a ser lida

    {opcionais segmento J}
    LerSegmentoJ52(PagFor.Lote.Last.SegmentoJ.Last.SegmentoJ52, nLinha);
    LerSegmentoJ99(PagFor.Lote.Last.SegmentoJ.Last.SegmentoJ99, nLinha);
//    LerSegmentoB(PagFor.Lote.Last.SegmentoJ.Last.SegmentoB, I);
//    LerSegmentoC(PagFor.Lote.Last.SegmentoJ.Last.SegmentoC, I);
    LerSegmentoZ(PagFor.Lote.Last.SegmentoJ.Last.SegmentoZ, nLinha);

    Linha := ArquivoTXT.Strings[nLinha+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
    RegOpc := LerCampo(Linha, 18, 2, tcStr);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

      GerarAvisos(CodOcorrencia, 'N', '', SeuNumero);
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
        GerarAvisos(CodOcorrencia, 'N', 'B', SeuNumero);
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

    GerarAvisos(CodOcorrencia, 'O', '', SeuNumero);
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

function TArquivoR_Itau.GetOcorrencia(aOcorrencia: TOcorrencia): String;
begin
  case aOcorrencia of
    to00: Result := 'PAGAMENTO EFETUADO';
    toAE: Result := 'DATA DE PAGAMENTO ALTERADA';
    toAG: Result := 'NÚMERO DO LOTE INVÁLIDO';
    toAH: Result := 'NÚMERO SEQUENCIAL DO REGISTRO NO LOTE INVÁLIDO';
    toAI: Result := 'PRODUTO DEMONSTRATIVO DE PAGAMENTO NÃO CONTRATADO';
    toAJ: Result := 'TIPO DE MOVIMENTO INVÁLIDO';
    toAL: Result := 'CÓDIGO DO BANCO FAVORECIDO INVÁLIDO';
    toAM: Result := 'AGÊNCIA DO FAVORECIDO INVÁLIDA';
    toAN: Result := 'CONTA CORRENTE DO FAVORECIDO INVÁLIDA';
    toAO: Result := 'NOME DO FAVORECIDO INVÁLIDO';
    toAP: Result := 'DATA DE PAGAMENTO / DATA DE VALIDADE / HORA DE LANÇAMENTO / ARRECADAÇÃO / APURAÇÃO INVÁLIDA';
    toAQ: Result := 'QUANTIDADE DE REGISTROS MAIOR QUE 999999';
    toAR: Result := 'VALOR ARRECADADO / LANÇAMENTO INVÁLIDO';
    toBC: Result := 'NOSSO NÚMERO INVÁLIDO';
    toBD: Result := 'PAGAMENTO AGENDADO';
    toBE: Result := 'PAGAMENTO AGENDADO COM FORMA ALTERADA PARA OP';
    toBI: Result := 'CNPJ / CPF DO FAVORECIDO NO SEGMENTO J-52 ou B INVÁLIDO / DOCUMENTO FAVORECIDO INVÁLIDO PIX';
    toBL: Result := 'VALOR DA PARCELA INVÁLIDO';
    toCD: Result := 'CNPJ / CPF INFORMADO DIVERGENTE DO CADASTRADO';
    toCE: Result := 'PAGAMENTO CANCELADO';
    toCF: Result := 'VALOR DO DOCUMENTO INVÁLIDO / VALOR DIVERGENTE DO QR CODE';
    toCG: Result := 'VALOR DO ABATIMENTO INVÁLIDO';
    toCH: Result := 'VALOR DO DESCONTO INVÁLIDO';
    toCI: Result := 'CNPJ / CPF / IDENTIFICADOR / INSCRIÇÃO ESTADUAL / INSCRIÇÃO NO CAD / ICMS INVÁLIDO';
    toCJ: Result := 'VALOR DA MULTA INVÁLIDO';
    toCK: Result := 'TIPO DE INSCRIÇÃO INVÁLIDA';
    toCL: Result := 'VALOR DO INSS INVÁLIDO';
    toCM: Result := 'VALOR DO COFINS INVÁLIDO';
    toCN: Result := 'CONTA NÃO CADASTRADA';
    toCO: Result := 'VALOR DE OUTRAS ENTIDADES INVÁLIDO';
    toCP: Result := 'CONFIRMAÇÃO DE OP CUMPRIDA';
    toCQ: Result := 'SOMA DAS FATURAS DIFERE DO PAGAMENTO';
    toCR: Result := 'VALOR DO CSLL INVÁLIDO';
    toCS: Result := 'DATA DE VENCIMENTO DA FATURA INVÁLIDA';
    toDA: Result := 'NÚMERO DE DEPEND. SALÁRIO FAMILIA INVALIDO';
    toDB: Result := 'NÚMERO DE HORAS SEMANAIS INVÁLIDO';
    toDC: Result := 'SALÁRIO DE CONTRIBUIÇÃO INSS INVÁLIDO';
    toDD: Result := 'SALÁRIO DE CONTRIBUIÇÃO FGTS INVÁLIDO';
    toDE: Result := 'VALOR TOTAL DOS PROVENTOS INVÁLIDO';
    toDF: Result := 'VALOR TOTAL DOS DESCONTOS INVÁLIDO';
    toDG: Result := 'VALOR LÍQUIDO NÃO NUMÉRICO';
    toDH: Result := 'VALOR LIQ. INFORMADO DIFERE DO CALCULADO';
    toDI: Result := 'VALOR DO SALÁRIO-BASE INVÁLIDO';
    toDJ: Result := 'BASE DE CÁLCULO IRRF INVÁLIDA';
    toDK: Result := 'BASE DE CÁLCULO FGTS INVÁLIDA';
    toDL: Result := 'FORMA DE PAGAMENTO INCOMPATÍVEL COM HOLERITE';
    toDM: Result := 'E-MAIL DO FAVORECIDO INVÁLIDO';
    toDV: Result := 'DOC / TED DEVOLVIDO PELO BANCO FAVORECIDO';
    toD0: Result := 'FINALIDADE DO HOLERITE INVÁLIDA';
    toD1: Result := 'MÊS DE COMPETENCIA DO HOLERITE INVÁLIDA';
    toD2: Result := 'DIA DA COMPETENCIA DO HOLETITE INVÁLIDA';
    toD3: Result := 'CENTRO DE CUSTO INVÁLIDO';
    toD4: Result := 'CAMPO NUMÉRICO DA FUNCIONAL INVÁLIDO';
    toD5: Result := 'DATA INÍCIO DE FÉRIAS NÃO NUMÉRICA';
    toD6: Result := 'DATA INÍCIO DE FÉRIAS INCONSISTENTE';
    toD7: Result := 'DATA FIM DE FÉRIAS NÃO NUMÉRICO';
    toD8: Result := 'DATA FIM DE FÉRIAS INCONSISTENTE';
    toD9: Result := 'NÚMERO DE DEPENDENTES IR INVÁLIDO';
    toEM: Result := 'CONFIRMAÇÃO DE OP EMITIDA';
    toEX: Result := 'DEVOLUÇÃO DE OP NÃO SACADA PELO FAVORECIDO';
    toE0: Result := 'TIPO DE MOVIMENTO HOLERITE INVÁLIDO';
    toE1: Result := 'VALOR 01 DO HOLERITE / INFORME INVÁLIDO';
    toE2: Result := 'VALOR 02 DO HOLERITE / INFORME INVÁLIDO';
    toE3: Result := 'VALOR 03 DO HOLERITE / INFORME INVÁLIDO';
    toE4: Result := 'VALOR 04 DO HOLERITE / INFORME INVÁLIDO';
    toFC: Result := 'PAGAMENTO EFETUADO ATRAVÉS DE FINANCIAMENTO COMPROR';
    toFD: Result := 'PAGAMENTO EFETUADO ATRAVÉS DE FINANCIAMENTO DESCOMPROR';
    toHA: Result := 'ERRO NO LOTE';
    toHM: Result := 'ERRO NO REGISTRO HEADER DE ARQUIVO';
    toIB: Result := 'VALOR DO DOCUMENTO INVÁLIDO';
    toIC: Result := 'VALOR DO ABATIMENTO INVÁLIDO';
    toID: Result := 'VALOR DO DESCONTO INVÁLIDO';
    toIE: Result := 'VALOR DA MORA INVÁLIDO';
    toIF: Result := 'VALOR DA MULTA INVÁLIDO';
    toIG: Result := 'VALOR DA DEDUÇÃO INVÁLIDO';
    toIH: Result := 'VALOR DO ACRÉSCIMO INVÁLIDO';
    toII: Result := 'DATA DE VENCIMENTO INVÁLIDA / QR CODE EXPIRADO';
    toIJ: Result := 'COMPETÊNCIA / PERÍODO REFERÊNCIA / PARCELA INVÁLIDA';
    toIK: Result := 'TRIBUTO NÃO LIQUIDÁVEL VIA SISPAG OU NÃO CONVENIADO COM ITAÚ';
    toIL: Result := 'CÓDIGO DE PAGAMENTO / EMPRESA /RECEITA INVÁLIDO';
    toIM: Result := 'TIPO X FORMA NÃO COMPATÍVEL';
    toIN: Result := 'BANCO/AGÊNCIA NÃO CADASTRADOS';
    toIO: Result := 'DAC / VALOR / COMPETÊNCIA / IDENTIFICADOR DO LACRE INVÁLIDO / IDENTIFICAÇÃO DO QR CODE INVÁLIDO';
    toIP: Result := 'DAC DO CÓDIGO DE BARRAS INVÁLIDO / ERRO NA VALIDAÇÃO DO QR CODE';
    toIQ: Result := 'DÍVIDA ATIVA OU NÚMERO DE ETIQUETA INVÁLIDO';
    toIR: Result := 'PAGAMENTO ALTERADO';
    toIS: Result := 'CONCESSIONÁRIA NÃO CONVENIADA COM ITAÚ';
    toIT: Result := 'VALOR DO TRIBUTO INVÁLIDO';
    toIU: Result := 'VALOR DA RECEITA BRUTA ACUMULADA INVÁLIDO';
    toIV: Result := 'NÚMERO DO DOCUMENTO ORIGEM / REFERÊNCIA INVÁLIDO';
    toIX: Result := 'CÓDIGO DO PRODUTO INVÁLIDO';
    toLA: Result := 'DATA DE PAGAMENTO DE UM LOTE ALTERADA';
    toLC: Result := 'LOTE DE PAGAMENTOS CANCELADO';
    toNA: Result := 'PAGAMENTO CANCELADO POR FALTA DE AUTORIZAÇÃO';
    toNB: Result := 'IDENTIFICAÇÃO DO TRIBUTO INVÁLIDA';
    toNC: Result := 'EXERCÍCIO (ANO BASE) INVÁLIDO';
    toND: Result := 'CÓDIGO RENAVAM NÃO ENCONTRADO/INVÁLIDO';
    toNE: Result := 'UF INVÁLIDA';
    toNF: Result := 'CÓDIGO DO MUNICÍPIO INVÁLIDO';
    toNG: Result := 'PLACA INVÁLIDA';
    toNH: Result := 'OPÇÃO/PARCELA DE PAGAMENTO INVÁLIDA';
    toNI: Result := 'TRIBUTO JÁ FOI PAGO OU ESTÁ VENCIDO';
    toNR: Result := 'OPERAÇÃO NÃO REALIZADA';
    toPD: Result := 'AQUISIÇÃO CONFIRMADA (EQUIVALE A OCORRÊNCIA 02 NO LAYOUT DE RISCO SACADO)';
    toRJ: Result := 'REGISTRO REJEITADO – CONTA EM PROCESSO DE ABERTURA OU BLOQUEADA';
    toRS: Result := 'PAGAMENTO DISPONÍVEL PARA ANTECIPAÇÃO NO RISCO SACADO – MODALIDADE RISCO SACADO PÓS AUTORIZADO';
    toSS: Result := 'PAGAMENTO CANCELADO POR INSUFICIÊNCIA DE SALDO / LIMITE DIÁRIO DE PAGTO EXCEDIDO';
    toTA: Result := 'LOTE NÃO ACEITO - TOTAIS DO LOTE COM DIFERENÇA';
    toTI: Result := 'TITULARIDADE INVÁLIDA';
    toX1: Result := 'FORMA INCOMPATÍVEL COM LAYOUT 010';
    toX2: Result := 'NÚMERO DA NOTA FISCAL INVÁLIDO';
    toX3: Result := 'IDENTIFICADOR DE NF/CNPJ INVÁLIDO';
    toX4: Result := 'FORMA 32 INVÁLIDA';
  else
    Result := '';
  end;
end;

end.

