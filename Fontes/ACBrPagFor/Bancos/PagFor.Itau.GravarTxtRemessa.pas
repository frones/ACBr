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

unit PagFor.Itau.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass, CNAB240.GravarTxtRemessa;

type
 { TArquivoW_Itau }

  TArquivoW_Itau = class(TArquivoW_CNAB240)
  protected
    procedure GeraRegistro0; override;

    procedure GeraRegistro1(I: Integer); override;

    procedure GeraRegistro5(I: Integer); override;

    procedure GeraRegistro9; override;

    procedure GeraSegmentoA(I: Integer); override;

    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); override;

    procedure GeraSegmentoC(mSegmentoCList: TSegmentoCList); override;

    procedure GeraSegmentoJ(I: Integer); override;

    procedure GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List); override;

    procedure GeraSegmentoN(mSegmentoN: TSegmentoN); override;

    procedure GeraSegmentoN1(I: Integer); override;

    procedure GeraSegmentoN2(I: Integer); override;

    procedure GeraSegmentoN3(I: Integer); override;

    procedure GeraSegmentoN4(I: Integer); override;

    procedure GeraSegmentoN567(I: Integer); override;

    procedure GeraSegmentoN8(I: Integer); override;

    procedure GeraSegmentoN9(I: Integer); override;

    procedure GeraSegmentoO(I: Integer); override;

    procedure GeraSegmentoW(mSegmentoWList: TSegmentoWList); override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrPagForConversao;

{ TArquivoW_Itau }

procedure TArquivoW_Itau.GeraRegistro0;
begin
  FpLinha := '';
  FQtdeRegistros := 1;
  FQtdeLotes := 0;
  FQtdeContasConc := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(0, 4, tcInt);
  GravarCampo(0, 1, tcInt);
  GravarCampo(' ', 6, tcStr);
  GravarCampo('081', 3, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Registro0.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(' ', 20, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(' ', 1, tcStr);

  if Trim(PagFor.Registro0.Empresa.ContaCorrente.DV) <> '' then
    GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.DV, 1, tcStr)
  else
    GravarCampo('0', 1, tcStr);

  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(0, 6, tcInt);
  GravarCampo('000', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);
  GravarCampo(' ', 69, tcStr);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_Itau.GeraRegistro1(I: Integer);
var
  Versao: string;
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeLotes);

  if PagFor.Lote.Items[I].Registro1.Servico.Operacao = toExtrato then
    Inc(FQtdeContasConc);

  FQtdeRegistrosLote := 1;
  FSequencialDeLote  := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo(1, 1, tcInt);
  GravarCampo(TpOperacaoToStr(PagFor.Lote.Items[I].Registro1.Servico.Operacao), 1, tcStr);
  GravarCampo(TpServicoToStr(PagFor.Lote.Items[I].Registro1.Servico.TipoServico), 2, tcStr);
  GravarCampo(FmLancamentoToStr(PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento), 2, tcStr);

  // Se for parte do Header (Pagamentos através de cheque, OP, DOC, TED e
  //   crédito em conta corrente)
  // Segmento A - Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
  // Segmento A - Pagamentos através de Nota Fiscal – Liquidação Eletrônica
  if PagFor.Lote.Items[I].SegmentoA.Count > 0 then
    Versao := '040'
  else
    Versao := '030';

  GravarCampo(Versao, 3, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(' ', 20, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(' ', 1, tcStr);

  if PadRight(TiraAcentos(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV), 1) <> ' ' then
    GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1, tcStr)
  else
    GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.DV, 1, tcStr);

  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Informacao1, 40, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Logradouro, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Numero, 5, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Complemento, 15, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Cidade, 20, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.CEP, 8, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Estado, 2, tcStr);
  GravarCampo(' ', 2, tcStr);
  GravarCampo(' ', 6, tcStr);
  GravarCampo(' ', 10, tcStr);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_Itau.GeraRegistro5(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeRegistrosLote);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo('5', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeRegistrosLote, 6, tcInt);

  if (PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento = flPagamentoConcessionarias) then
  begin
    // Contas de Concessionárias e Tributos com código de barras
    GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 18, tcDe2);
    GravarCampo(PagFor.Lote.Items[I].Registro5.QtdeMoeda, 15, tcDe8);
    GravarCampo(' ', 174, tcStr);
  end
  else
  if (PagFor.Lote.Items[I].Registro1.Servico.TipoServico = tsPagamentoSalarios) or
     (PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento in [flTributoGPS,
       flTributoDARFNormal, flTributoDARFSimples, flTributoGARESPICMS,
       flTributoDPVAT, flTributoIPVA, flTributoLicenciamento, flFGTSGFIP]) then
  begin
    // fgts
    GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 14, tcDe2);
    GravarCampo(PagFor.Lote.Items[I].Registro5.TotalOutrasEntidades, 14, tcDe2);
    GravarCampo(PagFor.Lote.Items[I].Registro5.TotalValorAcrescimo, 14, tcDe2);
    GravarCampo(PagFor.Lote.Items[I].Registro5.TotalValorArrecadado, 14, tcDe2);
    GravarCampo(' ', 151, tcStr);
  end
  else
  begin
    // Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
    // Liquidação de títulos (bloquetos) em cobrança no Itaú e em outros Bancos

    // informe de rendimento, este valor é 0
    if PadRight(PagFor.Lote.Items[I].Registro1.Informacao1, 2) = '06' then
      GravarCampo('0', 18, tcStrZero)
    else
      GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 18, tcDe2);

    GravarCampo('0', 18, tcStrZero);
    GravarCampo(' ', 171, tcStr);
  end;

  GravarCampo(' ', 10, tcStr);

  ValidarLinha('5');
  IncluirLinha;
end;

procedure TArquivoW_Itau.GeraRegistro9;
begin
  FpLinha := '';
  Inc(FQtdeRegistros);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(9999, 4, tcInt);
  GravarCampo('9', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeLotes, 6, tcInt);
  GravarCampo(FQtdeRegistros, 6, tcInt);
  GravarCampo(' ', 6, tcStr);
  GravarCampo(' ', 205, tcStr);

  ValidarLinha('9');
  IncluirLinha;
end;

procedure TArquivoW_Itau.GeraSegmentoA(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoA.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoA.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('A', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(Favorecido.Camara, 3, tcInt);
      GravarCampo(BancoToStr(Favorecido.Banco), 3, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Agencia.Codigo, 5, tcInt);
      GravarCampo(' ', 1, tcStr);

      if (PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento = flChequePagamento) or
         (PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento = flOPDisposicao) then
      begin
        GravarCampo(0, 12, tcInt);
        GravarCampo(' ', 1, tcStr);
        GravarCampo('0', 1, tcStr);
      end
      else
      begin
        GravarCampo(Favorecido.ContaCorrente.Conta.Numero, 12, tcInt64);

        if (Trim(Favorecido.ContaCorrente.Agencia.DV) <> '0') and
           (Trim(Favorecido.ContaCorrente.Conta.DV) <> '0') and
           (Length(Trim(Favorecido.ContaCorrente.Agencia.DV) +
                   Trim(Favorecido.ContaCorrente.Conta.DV)) > 1) then
          GravarCampo(Favorecido.ContaCorrente.Agencia.DV, 1, tcStr)
        else
          GravarCampo(' ', 1, tcStr);

        GravarCampo(Favorecido.ContaCorrente.Conta.DV, 1, tcStr);
      end;

      GravarCampo(Favorecido.Nome, 30, tcStr, True);
      GravarCampo(Credito.SeuNumero, 20, tcStr);
      GravarCampo(Credito.DataPagamento, 8, tcDat);
      GravarCampo('REA', 3, tcStr);
      //CÓDIGO ISPB IDENTIFICAÇÃO DA INSTITUIÇÃO PARA O SPB
      GravarCampo(CodigoISPB, 8, tcInt);
      // 9 = PIX
      if (Favorecido.Camara = 9) then
      begin
        // IDENTIFICAÇÃO DO TIPO DE TRANSFERÊNCIA
        GravarCampo(Favorecido.IDTipoTransfencia, 2, tcStrZero);
        GravarCampo(0, 5, tcInt);
      end
      else
       // ZEROS COMPLEMENTO DE REGISTRO
       GravarCampo(0, 7, tcInt);

      GravarCampo(Credito.ValorPagamento, 15, tcDe2);
      GravarCampo(Credito.NossoNumero, 20, tcStr);
      GravarCampo(Credito.DataReal, 8, tcDat);
      GravarCampo(Credito.ValorReal, 15, tcDe2);
      GravarCampo(Informacao2, 20, tcStr);
      GravarCampo(NumeroDocumento, 6, tcInt);
      GravarCampo(Favorecido.Inscricao.Numero, 14, tcStrZero);
      GravarCampo(CodigoDOC, 2, tcStr);
      GravarCampo(CodigoTED, 5, tcStr);
      GravarCampo(' ', 5, tcStr);
      GravarCampo(Aviso, 1, tcInt);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('A');
      IncluirLinha;

      {opcionais do segmento A}
      GeraSegmentoB(SegmentoB);
      GeraSegmentoC(SegmentoC);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
var
  J: Integer;
begin
  for J := 0 to mSegmentoBList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoBList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('B', 1, tcStr);

      if PixTipoChave <> tcpNenhum then
      begin
        GravarCampo(TipoChavePixToStr(PixTipoChave), 2, tcStr);
        GravarCampo(' ', 1, tcStr);
        GravarCampo(TpInscricaoToStr(Inscricao.Tipo), 1, tcStr);
        GravarCampo(Inscricao.Numero, 14, tcStrZero);
        GravarCampo(PixTXID, 30, tcStr);
        GravarCampo(PixMensagem, 65, tcStr);
        GravarCampo(PixChave, 100, tcStr);
        GravarCampo(' ', 3, tcStr);
        GravarCampo(' ', 10, tcStr);
      end
      else
      begin
        GravarCampo(' ', 3, tcStr);
        GravarCampo(TpInscricaoToStr(Inscricao.Tipo), 1, tcStr);
        GravarCampo(Inscricao.Numero, 14, tcStrZero);
        GravarCampo(Endereco.Logradouro, 30, tcStr, True);
        GravarCampo(Endereco.Numero, 5, tcStrZero);
        GravarCampo(Endereco.Complemento, 15, tcStr, True);
        GravarCampo(Endereco.Bairro, 15, tcStr, True);
        GravarCampo(Endereco.Cidade, 20, tcStr, True);
        GravarCampo(Endereco.CEP, 8, tcInt);
        GravarCampo(Endereco.Estado, 2, tcStr);
        GravarCampo(Email, 100, tcStr);
        GravarCampo(' ', 3, tcStr);
        GravarCampo(' ', 10, tcStr);

        {
        GravarCampo(DataVencimento, 8, tcDat);
        GravarCampo(Valor, 15, tcDe2);
        GravarCampo(Abatimento, 15, tcDe2);
        GravarCampo(Desconto, 15, tcDe2);
        GravarCampo(Mora, 15, tcDe2);
        GravarCampo(Multa, 15, tcDe2);
        GravarCampo(CodigoDOC, 15, tcStr);
        GravarCampo(Aviso, 1, tcInt);
        GravarCampo(CodigoUG, 6, tcInt);
        GravarCampo(' ', 8, tcStr);
        }
      end;

      ValidarLinha('B');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoC(mSegmentoCList: TSegmentoCList);
var
  J: Integer;
begin
  for J := 0 to mSegmentoCList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoCList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('C', 1, tcStr);
      GravarCampo(ValorCSLL, 15, tcDe2);
      GravarCampo(' ', 8, tcStr);
      GravarCampo(Vencimento, 8, tcDat);
      GravarCampo(ValorDocumento, 15, tcDe2);
      GravarCampo(ValorPIS, 15, tcDe2);
      GravarCampo(ValorIR, 15, tcDe2);
      GravarCampo(ValorISS, 15, tcDe2);
      GravarCampo(ValorCOFINS, 15, tcDe2);
      GravarCampo(Descontos, 15, tcDe2);
      GravarCampo(Abatimentos, 15, tcDe2);
      GravarCampo(Deducoes, 15, tcDe2);
      GravarCampo(Mora, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(Acrescimos, 15, tcDe2);
      GravarCampo(NumeroFaturaDocumento, 20, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('C');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoJ(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoJ.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoJ.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo('0', 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(CodigoBarras, 44, tcStr);
      GravarCampo(NomeCedente, 30, tcStr, True);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(ValorTitulo, 15, tcDe2);
      GravarCampo(Desconto, 15, tcDe2);
      GravarCampo(Acrescimo, 15, tcDe2);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(QtdeMoeda, 15, tcDe5);
      GravarCampo(ReferenciaSacado, 20, tcStr);
      GravarCampo(' ', 13, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('J');
      IncluirLinha;

      {opcionais segmento J}
      GeraSegmentoJ52(SegmentoJ52);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List);
var
  J: Integer;
begin
  for J := 0 to mSegmentoJ52List.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoJ52List.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo('52', 2, tcStr);
      GravarCampo(TpInscricaoToStr(Pagador.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Pagador.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Pagador.Nome, 40, tcStr, True);
      GravarCampo(TpInscricaoToStr(Beneficiario.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Beneficiario.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Beneficiario.Nome, 40, tcStr, True);

      if Chave = '' then
      begin
        GravarCampo(TpInscricaoToStr(SacadorAvalista.Inscricao.Tipo), 1, tcStr);
        GravarCampo(SacadorAvalista.Inscricao.Numero, 15, tcStrZero);
        GravarCampo(SacadorAvalista.Nome, 40, tcStr, True);
        GravarCampo(' ', 53, tcStr);
      end
      else
      begin
        GravarCampo(Chave, 77, tcStr);
        GravarCampo(TXID, 32, tcStr);
      end;

      ValidarLinha('J52');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN(mSegmentoN: TSegmentoN);
begin
  // Pagamento de Tributos e Impostos sem código de barras
  with mSegmentoN do
  begin
    Inc(FQtdeRegistros);
    Inc(FQtdeRegistrosLote);
    Inc(FSequencialDeLote);

    GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
    GravarCampo(FQtdeLotes, 4, tcInt);
    GravarCampo('3', 1, tcStr);
    GravarCampo(FSequencialDeLote, 5, tcInt);
    GravarCampo('N', 1, tcStr);
    GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
    GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN1(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN1.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN1.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(TpTributoToStr(ttGPS), 2, tcStr);
      GravarCampo(CodigoPagamentoGpsToStr(CodigoPagamento), 4, tcStr);
      GravarCampo(MesAnoCompetencia, 6, tcInt);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(ValorTributo, 14, tcDe2);
      GravarCampo(ValorOutrasEntidades, 14, tcDe2);
      GravarCampo(AtualizacaoMonetaria, 14, tcDe2);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);
      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(' ', 8, tcStr);
      GravarCampo(' ', 50, tcStr);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N1');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN2(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN2.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN2.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(TpTributoToStr(ttDARFNormal), 2, tcStr);
      GravarCampo(Receita, 4, tcInt);
      GravarCampo(TpInscricaoToStr(TipoContribuinte), 1, tcStr);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(Periodo, 8, tcDat);
      GravarCampo(Referencia, 17, tcStrZero);
      GravarCampo(ValorPrincipal, 14, tcDe2);
      GravarCampo(Multa, 14, tcDe2);
      GravarCampo(Juros, 14, tcDe2);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(' ', 30, tcStr);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N2');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN3(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN3.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN3.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(TpTributoToStr(ttDARFSimples), 2, tcStr);
      GravarCampo(Receita, 4, tcInt);
      GravarCampo(TpInscricaoToStr(TipoContribuinte), 1, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(Periodo, 8, tcDat);
      GravarCampo(ReceitaBruta, 9, tcDe2);
      GravarCampo(Percentual, 4, tcDe2);
      GravarCampo(' ', 4, tcStr);
      GravarCampo(ValorPrincipal, 14, tcDe2);
      GravarCampo(Multa, 14, tcDe2);
      GravarCampo(Juros, 14, tcDe2);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(' ', 30, tcStr);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N3');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN4(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN4.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN4.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(TpTributoToStr(ttGareICMS), 2, tcStr);
      GravarCampo(Receita, 4, tcInt);
      GravarCampo(TpInscricaoToStr(TipoContribuinte), 1, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(InscEst, 12, tcStrZero);
      GravarCampo(NumEtiqueta, 13, tcStrZero);
      GravarCampo(Referencia, 6, tcInt);
      GravarCampo(NumParcela, 13, tcStrZero);
      GravarCampo(ValorReceita, 14, tcDe2);
      GravarCampo(Juros, 14, tcDe2);
      GravarCampo(Multa, 14, tcDe2);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(' ', 11, tcStr);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N4');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN567(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN567.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN567.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      case Tributo of
        itIPVA:
          GravarCampo(TpTributoToStr(ttIPVA), 2, tcStr);

        itDPVAT:
          GravarCampo(TpTributoToStr(ttDPVAT), 2, tcStr);

        itLicenciamento:
          GravarCampo(TpTributoToStr(ttLicenciamento), 2, tcStr);
      else
        GravarCampo(' ', 2, tcStr);
      end;

      GravarCampo(' ', 4, tcStr);
      GravarCampo(TpInscricaoToStr(TipoContribuinte), 1, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(Exercicio, 4, tcInt);
      GravarCampo(Renavam, 9, tcStrZero);
      GravarCampo(Estado, 2, tcStr);

      if Tributo = itDPVAT then
        GravarCampo(0, 5, tcInt)
      else
        GravarCampo(Municipio, 5, tcInt);

      GravarCampo(Placa, 7, tcStr);
      GravarCampo(OpcaoPagamento, 1, tcStr);
      GravarCampo(ValorTributo, 14, tcDe2);
      GravarCampo(Desconto, 14, tcDe2);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);

      if Tributo = itDPVAT then
        GravarCampo(0, 8, tcInt)
      else
        GravarCampo(DataVencimento, 8, tcDat);

      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(' ', 29, tcStr);
      GravarCampo(NovoRenavam, 12, tcStrZero);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N567');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN8(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN8.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN8.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo('04', 2, tcStr); // DARJ
      GravarCampo(Receita, 4, tcInt);
      GravarCampo(TpInscricaoToStr(TipoContribuinte), 1, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(InscEst, 8, tcStrZero);
      GravarCampo(Origem, 16, tcStrZero);
      GravarCampo(' ', 1, tcStr);
      GravarCampo(ValorPrincipal, 14, tcDe2);
      GravarCampo(AtualizacaoMonetaria, 14, tcDe2);
      GravarCampo(Mora, 14, tcDe2);
      GravarCampo(Multa, 14, tcDe2);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(PeriodoParcela, 6, tcInt);
      GravarCampo(' ', 10, tcStr);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N8');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoN9(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN9.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN9.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo('11', 2, tcStr); // FGTS- GRF/GRRF/GRDE
      GravarCampo(Receita, 4, tcInt);

      // Não segue o padrão de codificação usado nos outros segmentos
      if TipoContribuinte = tiCNPJ then
        GravarCampo('1', 1, tcStr) // 1 = CNPJ
      else
        GravarCampo('2', 1, tcStr); // 2 = CEI

      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(CodigoBarras, 48, tcStr);
      GravarCampo(Identificador, 16, tcInt);
      GravarCampo(Lacre, 9, tcInt);
      GravarCampo(LacreDigito, 2, tcInt);
      GravarCampo(SegmentoN.NomeContribuinte, 30, tcStr);
      GravarCampo(SegmentoN.DataPagamento, 8, tcDat);
      GravarCampo(SegmentoN.ValorPagamento, 14, tcDe2);
      GravarCampo(' ', 30, tcStr);
      GravarCampo(SegmentoN.SeuNumero, 20, tcStr);
      GravarCampo(' ', 15, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N9');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoO(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoO.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoO.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('O', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(CodigoBarras, 48, tcStr);
      GravarCampo(NomeConcessionaria, 30, tcStr);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo('REA', 3, tcStr);
      GravarCampo(0, 15, tcInt);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(0, 15, tcInt);
      GravarCampo(' ', 3, tcStr);
      GravarCampo(0, 9, tcInt);
      GravarCampo(' ', 3, tcStr);
      GravarCampo(SeuNumero, 20, tcStr);
      GravarCampo(' ', 21, tcStr);
      GravarCampo(0, 15, tcInt);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('O');
      IncluirLinha;

      {opcionais segmento O}
      GeraSegmentoW(SegmentoW);
      GeraSegmentoZ(SegmentoZ);
      GeraSegmentoB(SegmentoB);
    end;
  end;
end;

procedure TArquivoW_Itau.GeraSegmentoW(mSegmentoWList: TSegmentoWList);
var
  J: Integer;
begin
  for J := 0 to mSegmentoWList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoWList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('W', 1, tcStr);
      GravarCampo(' ', 2, tcStr);
      GravarCampo(Informacoes1, 40, tcStr);
      GravarCampo(Informacoes2, 40, tcStr);
      GravarCampo(Informacoes3, 40, tcStr);
      GravarCampo(Informacoes4, 40, tcStr);
      GravarCampo(' ', 64, tcStr);

      ValidarLinha('W');
      IncluirLinha;
    end;
  end;
end;

end.
