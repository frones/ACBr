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

unit PagFor.HSBC.GravarTxtRemessa;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass, CNAB240.GravarTxtRemessa;

type
 { TArquivoW_HSBC }

  TArquivoW_HSBC = class(TArquivoW_CNAB240)
  protected
    procedure GeraRegistro0; override;

    procedure GeraRegistro1(I: Integer); override;

    procedure GeraRegistro5(I: Integer); override;

    procedure GeraRegistro9; override;

    procedure GeraSegmentoA(I: Integer); override;

    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); override;

    procedure GeraSegmentoJ(I: Integer); override;

    procedure GeraSegmentoO(I: Integer); override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrPagForConversao;

{ TArquivoW_HSBC }

procedure TArquivoW_HSBC.GeraRegistro0;
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
  GravarCampo(PagFor.Registro0.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(PagFor.Registro0.Arquivo.Sequencia, 6, tcInt);
  // HSBC ’020” ou  “030”
  GravarCampo('020', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);
  // Fixo "CPG"
  GravarCampo('CPG', 3, tcStr);
  // Uso exclusivo das vans
  GravarCampo('Y2K', 3, tcStr);
  // Codigo do Servico
  GravarCampo(' ', 14, tcStr);
  GravarCampo(' ', 49, tcStr);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_HSBC.GeraRegistro1(I: Integer);
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
  GravarCampo('020', 3, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero, 12, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Informacao1, 40, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Logradouro, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Numero, 5, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Complemento, 15, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Cidade, 20, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.CEP, 8, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Estado, 2, tcStr);
  // Comprovante de Pagamento Emissão em Lote  “S=Sim” ou “N=Não”
  GravarCampo('S', 1, tcStr);
  GravarCampo(' ', 17, tcStr);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_HSBC.GeraRegistro5(I: Integer);
begin
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeRegistrosLote);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo('5', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeRegistrosLote, 6, tcInt);
  GravarCampo(' ', 3, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 15, tcDe2);
  GravarCampo(' ', 199, tcStr);

  ValidarLinha('5');
  IncluirLinha;
end;

procedure TArquivoW_HSBC.GeraRegistro9;
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

procedure TArquivoW_HSBC.GeraSegmentoA(I: Integer);
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
      GravarCampo(Favorecido.ContaCorrente.Conta.Numero, 12, tcInt);
      GravarCampo(Favorecido.ContaCorrente.Conta.DV, 1, tcStr);
      GravarCampo(' ', 1, tcStr);
      GravarCampo(Favorecido.Nome, 30, tcStr, True);
      GravarCampo(Credito.SeuNumero, 16, tcStr);
      GravarCampo(' ', 4, tcStr);
      GravarCampo(Credito.DataPagamento, 8, tcDat);
      GravarCampo('R$ ', 3, tcStr);
      GravarCampo(' ', 17, tcStr);
      GravarCampo(Credito.ValorPagamento, 13, tcDe2);
      GravarCampo('N', 1, tcStr);
      GravarCampo(' ', 30, tcStr);
      GravarCampo(' ', 12, tcStr);
      GravarCampo(Informacao2, 40, tcStr);
      GravarCampo(CodigoDOC, 2, tcStr);
      GravarCampo(CodigoTED, 5, tcStr);
      GravarCampo(CodigoComp, 2, tcStr);
      GravarCampo(' ', 3, tcStr);
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

procedure TArquivoW_HSBC.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
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
      GravarCampo(' ', 105, tcStr);

      if CodigoUG > 0 then
        GravarCampo(CodigoUG, 8, tcInt)
      else
        GravarCampo(CodigoDOC, 8, tcStr);

      ValidarLinha('B');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_HSBC.GeraSegmentoJ(I: Integer);
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
      GravarCampo(' ', 2, tcStr);
      GravarCampo(ValorTitulo, 13, tcDe2);
      GravarCampo(' ', 2, tcStr);
      GravarCampo(Desconto, 13, tcDe2);
      GravarCampo(' ', 2, tcStr);
      GravarCampo(Acrescimo, 13, tcDe2);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(' ', 2, tcStr);
      GravarCampo(ValorPagamento, 13, tcDe2);
      GravarCampo(' ', 2, tcStr);
      GravarCampo(QtdeMoeda, 13, tcDe5);
      GravarCampo(ReferenciaSacado, 20, tcStr);
      GravarCampo(' ', 20, tcStr);
      GravarCampo(' ', 2, tcStr);
      // Comprovante de Pagto. Emissão Individual “S=Sim ou N=Não”
      GravarCampo('N', 1, tcStr);
      GravarCampo(' ', 15, tcStr);

      ValidarLinha('J');
      IncluirLinha;

      {opcionais segmento J}
      GeraSegmentoJ52(SegmentoJ52);
    end;
  end;
end;

procedure TArquivoW_HSBC.GeraSegmentoO(I: Integer);
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
      GravarCampo(NomeConcessionaria, 30, tcStr);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(SeuNumero, 20, tcStr);
      GravarCampo(NossoNumero, 19, tcStr);
      // Comprovante Pagamento Emissão Individual  “S = Sim” ou “N = Não”
      GravarCampo('N', 1, tcStr);
      GravarCampo(' ', 68, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('O');
      IncluirLinha;

      {opcionais segmento O}
      GeraSegmentoW(SegmentoW);
      GeraSegmentoZ(SegmentoZ);
      GeraSegmentoB(SegmentoB);
    end;
  end;
end;

end.
