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

unit PagFor.Bradesco.LerTxtRetorno;

interface

uses
  SysUtils, Classes,
  CNAB240.LerTxtRetorno, ACBrPagForClass;

type
 { TArquivoR_Bradesco }

  TArquivoR_Bradesco = class(TArquivoR_CNAB240)
  protected
    procedure LerRegistro1(I: Integer); override;

    procedure LerSegmentoA(I: Integer); override;

    procedure LerSegmentoJ(I: Integer; var LeuRegistroJ: boolean); override;
  end;

implementation

uses
  ACBrPagForConversao;

{ TArquivoR_Bradesco }

procedure TArquivoR_Bradesco.LerRegistro1(I: Integer);
var
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[I];

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
    DescOcorrencia := DescricaoRetorno(CodOcorrencia);

    GerarAvisos(CodOcorrencia, DescOcorrencia, '1', '', '');
  end;
end;

procedure TArquivoR_Bradesco.LerSegmentoA(I: Integer);
var
  mOk: Boolean;
  RegSeg: string;
  x: Integer;
begin
  Linha := ArquivoTXT.Strings[I];
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
    DescOcorrencia := DescricaoRetorno(CodOcorrencia);

    GerarAvisos(CodOcorrencia, DescOcorrencia, 'A', '', Credito.SeuNumero);
  end;

  Linha := ArquivoTXT.Strings[I+1];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  while Pos(RegSeg, '3B/3C/3D/3E/3F/3Z/') > 0 do
  begin
    Inc(I); //próxima linha do txt a ser lida
    {opcionais do segmento A}
    LerSegmentoB(PagFor.Lote.Last.SegmentoA.Last.SegmentoB, I);
    LerSegmentoC(PagFor.Lote.Last.SegmentoA.Last.SegmentoC, I);
//    LerSegmentoE(PagFor.Lote.Last.SegmentoA.Last.SegmentoE, I);
//    LerSegmentoF(PagFor.Lote.Last.SegmentoA.Last.SegmentoF, I);
    LerSegmentoZ(PagFor.Lote.Last.SegmentoA.Last.SegmentoZ, I);

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

    for x := 0 to PagFor.Lote.Last.SegmentoA.Last.SegmentoZ.Count - 1 do
    begin
      with PagFor.Lote.Last.SegmentoA.Last.SegmentoZ.Items[x] do
      begin
        GerarAvisos(CodOcorrencia, DescOcorrencia, 'A', 'Z',
          PagFor.Lote.Last.SegmentoA.Last.Credito.SeuNumero);
      end;
    end;

    Linha := ArquivoTXT.Strings[I+1];
    RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);
  end;
end;

procedure TArquivoR_Bradesco.LerSegmentoJ(I: Integer;
  var LeuRegistroJ: boolean);
var
  RegOpc: string;
begin
  RegOpc := LerCampo(Linha, 18, 2, tcStr);

  if RegOpc = '52' then
    Exit;

  inherited LerSegmentoJ(I, LeuRegistroJ);
end;

end.

