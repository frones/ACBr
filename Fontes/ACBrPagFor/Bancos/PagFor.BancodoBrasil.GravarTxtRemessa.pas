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

unit PagFor.BancodoBrasil.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass, ACBrPagForConversao, CNAB240.GravarTxtRemessa;

type
 { TArquivoW_BancodoBrasil }

  TArquivoW_BancodoBrasil = class(TArquivoW_CNAB240)
  private
    function InscricaoToStr(const t: TTipoInscricao): String;
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

    procedure GeraSegmentoN1(I: Integer); override;

    procedure GeraSegmentoN2(I: Integer); override;

    procedure GeraSegmentoN3(I: Integer); override;

    procedure GeraSegmentoN4(I: Integer); override;

    procedure GeraSegmentoN567(I: Integer); override;

    procedure GeraSegmentoN8(I: Integer); override;

    procedure GeraSegmentoO(I: Integer); override;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TArquivoW_BancodoBrasil }

procedure TArquivoW_BancodoBrasil.GeraRegistro0;
begin
  FpLinha := '';
  FQtdeRegistros := 1;
  FQtdeLotes := 0;
  FQtdeContasConc := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(0, 4, tcInt);
  GravarCampo(0, 1, tcInt);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Registro0.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Registro0.Empresa.Convenio, 9, tcStrZero);
  GravarCampo(126, 4, tcInt);
  GravarCampo(' ', 5, tcStr);

  if PagFor.Registro0.RemessaTeste then
    GravarCampo('TS', 2, tcStr)
  else
    GravarCampo(' ', 2, tcStr);

  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo('0', 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(PagFor.Registro0.Arquivo.Sequencia, 6, tcInt);
  GravarCampo('003', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);
  GravarCampo(PagFor.Registro0.ReservadoBanco, 20, tcStr);
  GravarCampo(PagFor.Registro0.ReservadoEmpresa, 20, tcStr);
  GravarCampo(' ', 11, tcStr);
  GravarCampo(' ', 3, tcStr);
  GravarCampo(0, 3, tcInt);
  GravarCampo(0, 2, tcInt);
  GravarCampo(0, 10, tcInt);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_BancodoBrasil.GeraRegistro1(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeLotes);

  if PagFor.Lote.Items[I].Registro1.Servico.Operacao = toExtrato then
    Inc(FQtdeContasConc);

  FQtdeRegistrosLote := 0;
  FSequencialDeLote  := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo(1, 1, tcInt);
  GravarCampo(TpOperacaoToStr(PagFor.Lote.Items[I].Registro1.Servico.Operacao), 1, tcStr);
  GravarCampo(TpServicoToStr(PagFor.Lote.Items[I].Registro1.Servico.TipoServico), 2, tcStr);
  GravarCampo(FmLancamentoToStr(PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento), 2, tcStr);
  GravarCampo('003', 3, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Registro0.Empresa.Convenio, 9, tcStrZero);
  GravarCampo(126, 4, tcInt);
  GravarCampo(' ', 5, tcStr);

  if PagFor.Registro0.RemessaTeste then
    GravarCampo('TS', 2, tcStr)
  else
    GravarCampo(' ', 2, tcStr);

  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo('0', 1, tcStrZero);
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
  GravarCampo(0, 10, tcInt);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_BancodoBrasil.GeraRegistro5(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeRegistrosLote);
  // Soma + 1 pois foi inicado com 0 no regstro1
  Inc(FQtdeRegistrosLote);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo('5', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeRegistrosLote, 6, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 18, tcDe2);
  GravarCampo('0', 18, tcStrZero);
  GravarCampo('0', 6, tcStrZero);
  GravarCampo(' ', 165, tcStr);
  GravarCampo('0', 10, tcStrZero);

  ValidarLinha('5');
  IncluirLinha;
end;

procedure TArquivoW_BancodoBrasil.GeraRegistro9;
begin
  FpLinha := '';
  Inc(FQtdeRegistros);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(9999, 4, tcInt);
  GravarCampo('9', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeLotes, 6, tcInt);
  GravarCampo(FQtdeRegistros, 6, tcInt);
  GravarCampo('0', 6, tcStrZero);
  GravarCampo(' ', 205, tcStr);

  ValidarLinha('9');
  IncluirLinha;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoA(I: Integer);
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
      GravarCampo(Favorecido.ContaCorrente.Agencia.DV, 1, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Conta.Numero, 12, tcInt);
      GravarCampo(Favorecido.ContaCorrente.Conta.DV, 1, tcStr);
      GravarCampo(Favorecido.ContaCorrente.DV, 1, tcStr);
      GravarCampo(Favorecido.Nome, 30, tcStr, True);
      GravarCampo(Credito.SeuNumero, 20, tcStr);
      GravarCampo(Credito.DataPagamento, 8, tcDat);
      GravarCampo(TpMoedaToStr(Credito.Moeda.Tipo), 3, tcStr);
      GravarCampo(0, 15, tcDe5);
      GravarCampo(Credito.ValorPagamento, 15, tcDe2);
      GravarCampo(Credito.NossoNumero, 20, tcStr);
      GravarCampo(0, 8, tcDat);
      GravarCampo(0, 15, tcDe2);
      GravarCampo(Informacao2, 40, tcStr);
      GravarCampo(CodigoDOC, 2, tcStr);
      GravarCampo(CodigoTED, 5, tcStr);
      GravarCampo(' ', 5, tcStr);
      GravarCampo(Aviso, 1, tcInt);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('A');
      IncluirLinha;

      {opcionais do segmento A}
      GeraSegmentoB(SegmentoB);
      GeraSegmentoC(SegmentoC);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
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
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('B', 1, tcStr);
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

      ValidarLinha('B');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoC(mSegmentoCList: TSegmentoCList);
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
      GravarCampo(' ', 3, tcStr);
      GravarCampo(ValorIR, 15, tcDe2);
      GravarCampo(ValorISS, 15, tcDe2);
      GravarCampo(ValorIOF, 15, tcDe2);
      GravarCampo(Deducoes, 15, tcDe2);
      GravarCampo(Acrescimos, 15, tcDe2);
      GravarCampo(Substituta.ContaCorrente.Agencia.Codigo, 5, tcInt);
      GravarCampo(Substituta.ContaCorrente.Agencia.DV, 1, tcStr);
      GravarCampo(Substituta.ContaCorrente.Conta.Numero, 12, tcInt);
      GravarCampo(Substituta.ContaCorrente.Conta.DV, 1, tcStr);
      GravarCampo(Substituta.ContaCorrente.DV, 1, tcStr);
      GravarCampo(ValorINSS, 15, tcDe2);
      GravarCampo(' ', 113, tcStr);

      ValidarLinha('C');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoJ(I: Integer);
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
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(CodigoBarras, 44, tcStr);
      GravarCampo(NomeCedente, 30, tcStr, True);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(ValorTitulo, 15, tcDe2);
      GravarCampo(Desconto, 15, tcDe2);
      GravarCampo(Acrescimo, 15, tcDe2);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(0, 15, tcDe2);
      GravarCampo(ReferenciaSacado, 20, tcStr);
      GravarCampo(NossoNumero, 20, tcStr);
      GravarCampo(CodigoMoeda, 2, tcInt);
      GravarCampo(' ', 6, tcStr);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('J');
      IncluirLinha;

      {opcionais segmento J}
      GeraSegmentoJ52(SegmentoJ52);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoJ52(
  mSegmentoJ52List: TSegmentoJ52List);
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
      Inc(FSequencialDeLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDeLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(' ', 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo('52', 2, tcStr);
      GravarCampo(TpInscricaoToStr(Pagador.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Pagador.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Pagador.Nome, 40, tcStr, True);
      GravarCampo(TpInscricaoToStr(Beneficiario.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Beneficiario.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Beneficiario.Nome, 40, tcStr, True);
      GravarCampo(TpInscricaoToStr(SacadorAvalista.Inscricao.Tipo), 1, tcStr);
      GravarCampo(SacadorAvalista.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(SacadorAvalista.Nome, 40, tcStr, True);
      GravarCampo(' ', 53, tcStr);

      ValidarLinha('J52');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoN1(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN1.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN1.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo('17', 2, tcStr);
      GravarCampo(Competencia, 6, tcInt);
      GravarCampo(ValorTributo, 15, tcDe2);
      GravarCampo(ValorOutrasEntidades, 15, tcDe2);
      GravarCampo(AtualizacaoMonetaria, 15, tcDe2);
      GravarCampo(' ', 45, tcStr);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('N1');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoN2(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN2.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN2.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo('16', 2, tcStr);
      GravarCampo(Periodo, 8, tcDat);
      GravarCampo(Referencia, 17, tcStrZero);
      GravarCampo(ValorPrincipal, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(Juros, 15, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(' ', 18, tcStr);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('N2');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoN3(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN3.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN3.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo('18', 2, tcStr);
      GravarCampo(Periodo, 8, tcDat);
      GravarCampo(ReceitaBruta, 15, tcDe2);
      GravarCampo(Percentual, 7, tcDe2);
      GravarCampo(ValorPrincipal, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(Juros, 15, tcDe2);
      GravarCampo(' ', 21, tcStr);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('N3');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoN4(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN4.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN4.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(TpIndTributoToStr(PagFor.Geral.idTributo), 2, tcStr);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(InscEst, 12, tcStrZero);
      GravarCampo(NumEtiqueta, 13, tcStrZero);
      GravarCampo(Referencia, 6, tcInt);
      GravarCampo(NumParcela, 13, tcStrZero);
      GravarCampo(ValorReceita, 15, tcDe2);
      GravarCampo(Multa, 14, tcDe2);
      GravarCampo(Juros, 14, tcDe2);
      GravarCampo(' ', 1, tcStr);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('N4');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoN567(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN567.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN567.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(TpIndTributoToStr(PagFor.Geral.idTributo), 2, tcStr);
      GravarCampo(Exercicio, 4, tcInt);
      GravarCampo(Renavam, 9, tcStrZero);
      GravarCampo(Estado, 2, tcStr);
      GravarCampo(Municipio, 5, tcInt);
      GravarCampo(Placa, 7, tcStr);

      case PagFor.Geral.idTributo of
        itIPVA:
          begin
            GravarCampo(OpcaoPagamento, 1, tcStr);
            GravarCampo(NovoRenavam, 12, tcStrZero);
            GravarCampo(' ', 55, tcStr);
          end;

        itDPVAT:
          begin
            GravarCampo('5', 1, tcStr);
            GravarCampo(NovoRenavam, 12, tcStrZero);
            GravarCampo(' ', 55, tcStr);
          end;

        itLicenciamento:
          begin
            GravarCampo('5', 1, tcStr);
            GravarCampo(OpcaoRetirada, 1, tcStr);
            GravarCampo(NovoRenavam, 12, tcStrZero);
            GravarCampo(' ', 54, tcStr);
          end;
      else
        begin
          GravarCampo(' ', 1, tcStr);
          GravarCampo(NovoRenavam, 12, tcStrZero);
          GravarCampo(' ', 55, tcStr);
        end;
      end;

      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('N567');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoN8(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN8.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN8.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(InscEst, 8, tcStrZero);
      GravarCampo(Origem, 16, tcStrZero);
      GravarCampo(ValorPrincipal, 15, tcDe2);
      GravarCampo(AtualizacaoMonetaria, 15, tcDe2);
      GravarCampo(Mora, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(PeriodoParcela, 6, tcInt);
      GravarCampo('0', 10, tcStrZero);

      ValidarLinha('N8');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_BancodoBrasil.GeraSegmentoO(I: Integer);
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
      GravarCampo(CodigoBarras, 44, tcStr);
      GravarCampo(NomeConcessionaria, 30, tcStr, True);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(SeuNumero, 20, tcStr);
      GravarCampo(NossoNumero, 20, tcStr);
      GravarCampo(' ', 68, tcStr);
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

function TArquivoW_BancodoBrasil.InscricaoToStr(
  const t: TTipoInscricao): String;
begin
 result := EnumeradoToStr(t, ['1', '2', '3', '9'],
                             [tiCNPJ, tiCPF, tiPISPASEP, tiOutros]);
end;

end.
