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

unit PagFor.Sicredi.LerTxtRetorno;

interface

uses
  SysUtils, Classes,
  CNAB240.LerTxtRetorno, ACBrPagForClass;

type
 { TArquivoR_Sicredi }

  TArquivoR_Sicredi = class(TArquivoR_CNAB240)
  protected
    procedure LerSegmentoA(nLinha: Integer); override;

    procedure LerSegmentoB(mSegmentoBList: TSegmentoBList; nLinha: Integer); override;

    procedure LerSegmentoG(nLinha: Integer); override;

    procedure LerSegmentoH(mSegmentoHList: TSegmentoHList; nLinha: Integer); override;

    procedure LerSegmentoN1(nLinha: Integer); override;

    procedure LerSegmentoN2(nLinha: Integer); override;

    procedure LerSegmentoN3(nLinha: Integer); override;

    procedure LerSegmentoN4(nLinha: Integer); override;

    procedure LerSegmentoN567(nLinha: Integer); override;

    procedure LerSegmentoN8(nLinha: Integer); override;
  end;

implementation

uses
  ACBrPagForConversao;

{ TArquivoR_Sicredi }

procedure TArquivoR_Sicredi.LerSegmentoA(nLinha: Integer);
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

procedure TArquivoR_Sicredi.LerSegmentoB(mSegmentoBList: TSegmentoBList;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3B') then
    Exit;

  mSegmentoBList.New;

  with mSegmentoBList.Last do
  begin
    if LerCampo(Linha, 15, 2, tcStr) <> '' then
    begin
      // PIX
      Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));
      Inscricao.Numero := LerCampo(Linha, 19, 14, tcStr);

      PixTipoChave := StrToTipoChavePIX(mOk, LerCampo(Linha, 15, 2, tcStr));
      PixMensagem := LerCampo(Linha, 128, 99, tcStr);
      CodigoUG := LerCampo(Linha, 227, 6, tcInt);
      CodigoISPB := LerCampo(Linha, 233, 8, tcInt);
    end
    else
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

      DataVencimento := LerCampo(Linha, 128, 8, tcDat);
      Valor := LerCampo(Linha, 136, 15, tcDe2);
      Abatimento := LerCampo(Linha, 151, 15, tcDe2);
      Desconto := LerCampo(Linha, 166, 15, tcDe2);
      Mora := LerCampo(Linha, 181, 15, tcDe2);
      Multa := LerCampo(Linha, 196, 15, tcDe2);
      CodigoDoc := LerCampo(Linha, 211, 15, tcStr);
    end;
  end;
end;

procedure TArquivoR_Sicredi.LerSegmentoG(nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3G') then
    Exit;

  PagFor.Lote.Last.SegmentoG.New;

  with PagFor.Lote.Last.SegmentoG.Last do
  begin
    CodigoBarras := LerCampo(Linha, 18, 44, tcStr);
    Cedente.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 62, 1, tcStr));

    if Cedente.Inscricao.Tipo = tiCNPJ then
      Cedente.Inscricao.Numero := LerCampo(Linha, 64, 14, tcStr)
    else
      Cedente.Inscricao.Numero := LerCampo(Linha, 67, 11, tcStr);

    Cedente.Nome := LerCampo(Linha, 78, 30, tcStr);

    Vencimento := LerCampo(Linha, 108, 8, tcDatISO);
    ValorTitulo := LerCampo(Linha, 116, 15, tcDe2);
    QtdeMoeda := LerCampo(Linha, 131, 15, tcDe5);
    CodigoMoeda := LerCampo(Linha, 146, 2, tcInt);
    NumeroDocumento := LerCampo(Linha, 148, 15, tcStr);
    AgenciaCobradora := LerCampo(Linha, 163, 5, tcInt);
    DVCobradora := LerCampo(Linha, 168, 1, tcStr);
    Praca := LerCampo(Linha, 169, 10, tcStr);
    Carteira := LerCampo(Linha, 179, 1, tcStr);
    EspecieTitulo := LerCampo(Linha, 180, 2, tcInt);
    DataEmissao := LerCampo(Linha, 182, 8, tcDatISO);
    JurosMora := LerCampo(Linha, 190, 15, tcDe2);

    //Em algumas situações o banco manda tudo como 999... ou 555...
    //Multa não pode ultrapassar 10%
    if JurosMora > (ValorTitulo * 0.1) then
      JurosMora := 0;

    Desconto1.Codigo := LerCampo(Linha, 205, 1, tcInt);
    Desconto1.Data := LerCampo(Linha, 206, 8, tcDatISO);
    Desconto1.Valor := LerCampo(Linha, 114, 15, tcDe2);

    CodigoProtesto := LerCampo(Linha, 229, 1, tcInt);
    PrazoProtesto := LerCampo(Linha, 230, 2, tcInt);
    DataLimite := LerCampo(Linha, 232, 8, tcDatISO);
  end;

  LerSegmentoH(PagFor.Lote.Last.SegmentoG.Last.SegmentoH, nLinha + 1);
end;

procedure TArquivoR_Sicredi.LerSegmentoH(mSegmentoHList: TSegmentoHList;
  nLinha: Integer);
var
  mOk: Boolean;
  RegSeg: string;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if (RegSeg <> '3H') then
    Exit;

  mSegmentoHList.New;

  with mSegmentoHList.Last do
  begin
    Avalista.Inscricao.Tipo := StrToTpInscricao(mOk, LerCampo(Linha, 18, 1, tcStr));

    if Avalista.Inscricao.Tipo = tiCNPJ then
      Avalista.Inscricao.Numero := LerCampo(Linha, 20, 14, tcStr)
    else
      Avalista.Inscricao.Numero := LerCampo(Linha, 23, 11, tcStr);

    Avalista.Nome := LerCampo(Linha, 34, 40, tcStr);

    Desconto2.Codigo := LerCampo(Linha, 74, 1, tcInt);
    Desconto2.Data := LerCampo(Linha, 75, 8, tcDatISO);
    Desconto2.Valor := LerCampo(Linha, 83, 15, tcDe2);

    Desconto3.Codigo := LerCampo(Linha, 98, 1, tcInt);
    Desconto3.Data := LerCampo(Linha, 99, 8, tcDatISO);
    Desconto3.Valor := LerCampo(Linha, 107, 15, tcDe2);

    Multa.Codigo := LerCampo(Linha, 122, 1, tcInt);
    Multa.Data := LerCampo(Linha, 123, 8, tcDatISO);
    Multa.Valor := LerCampo(Linha, 131, 15, tcDe2);

    Abatimento := LerCampo(Linha, 146, 15, tcDe2);
    Informacao1 := LerCampo(Linha, 161, 40, tcStr);
    Informacao2 := LerCampo(Linha, 201, 40, tcStr);
  end;
end;

procedure TArquivoR_Sicredi.LerSegmentoN1(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N1 - GPS
  if LerCampo(Linha, 133, 2, tcInt) <> 17 then
    Exit;

  PagFor.Lote.Last.SegmentoN1.New;

  with PagFor.Lote.Last.SegmentoN1.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := StrToIntDef(LerCampo(Linha, 111, 6, tcStr), 0);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    MesAnoCompetencia := LerCampo(Linha, 135, 6, tcInt);
    ValorTributo := LerCampo(Linha, 141, 15, tcDe2);
    ValorOutrasEntidades := LerCampo(Linha, 156, 15, tcDe2);
    AtualizacaoMonetaria := LerCampo(Linha, 171, 15, tcDe2);
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

procedure TArquivoR_Sicredi.LerSegmentoN2(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N2 - DARF NORMAL
  if LerCampo(Linha, 133, 2, tcInt) <> 16 then
    Exit;

  PagFor.Lote.Last.SegmentoN2.New;

  with PagFor.Lote.Last.SegmentoN2.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := StrToIntDef(LerCampo(Linha, 111, 6, tcStr), 0);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Periodo := LerCampo(Linha, 135, 8, tcDat);
    Referencia := LerCampo(Linha, 143, 17, tcStr);
    ValorPrincipal := LerCampo(Linha, 160, 15, tcDe2);
    Multa := LerCampo(Linha, 175, 15, tcDe2);
    Juros := LerCampo(Linha, 190, 15, tcDe2);
    DataVencimento := LerCampo(Linha, 205, 8, tcDat);
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

procedure TArquivoR_Sicredi.LerSegmentoN3(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N3 - DARF SIMPLES
  if LerCampo(Linha, 133, 2, tcInt) <> 18 then
    Exit;

  PagFor.Lote.Last.SegmentoN3.New;

  with PagFor.Lote.Last.SegmentoN3.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := StrToIntDef(LerCampo(Linha, 111, 6, tcStr), 0);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Periodo := LerCampo(Linha, 135, 8, tcDat);
    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
    Percentual := LerCampo(Linha, 158, 7, tcDe2);
    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
    Juros := LerCampo(Linha, 195, 15, tcDe2);
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

procedure TArquivoR_Sicredi.LerSegmentoN4(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
  Tributo: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N4 - GARE-SP (ICMS/DR/ITCMD)
  Tributo := LerCampo(Linha, 133, 2, tcInt);

  if  not (Tributo in [22, 23, 24]) then
    Exit;

  PagFor.Lote.Last.SegmentoN4.New;

  with PagFor.Lote.Last.SegmentoN4.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := StrToIntDef(LerCampo(Linha, 111, 6, tcStr), 0);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
    Juros := LerCampo(Linha, 195, 15, tcDe2);
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

procedure TArquivoR_Sicredi.LerSegmentoN567(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
  Tributo: Integer;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N567 - IPVA, DPVAT,  LICENCIAMENTO
  Tributo := LerCampo(Linha, 133, 2, tcInt);

  if  not (Tributo in [25, 27, 26]) then
    Exit;

  PagFor.Lote.Last.SegmentoN567.New;

  with PagFor.Lote.Last.SegmentoN567.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := StrToIntDef(LerCampo(Linha, 111, 6, tcStr), 0);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
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

procedure TArquivoR_Sicredi.LerSegmentoN8(nLinha: Integer);
var
  RegSeg: string;
  x: Integer;
  mOk: Boolean;
begin
  Linha := ArquivoTXT.Strings[nLinha];
  RegSeg := LerCampo(Linha, 8, 1, tcStr) + LerCampo(Linha, 14, 1, tcStr);

  if RegSeg <> '3N' then
    Exit;

  //Tributo N8 - DARJ
  if LerCampo(Linha, 133, 2, tcInt) <> 21 then
    Exit;

  PagFor.Lote.Last.SegmentoN8.New;

  with PagFor.Lote.Last.SegmentoN8.Last do
  begin
    LerSegmentoN(SegmentoN);

    Receita := StrToIntDef(LerCampo(Linha, 111, 6, tcStr), 0);
    TipoContribuinte := StrToTpInscricao(mOk, LerCampo(Linha, 117, 2, tcStr));
    idContribuinte := LerCampo(Linha, 119, 14, tcStr);
//    Periodo := LerCampo(Linha, 135, 8, tcDat);
//    ReceitaBruta := LerCampo(Linha, 143, 15, tcDe2);
//    Percentual := LerCampo(Linha, 158, 7, tcDe2);
    ValorPrincipal := LerCampo(Linha, 165, 15, tcDe2);
    Multa := LerCampo(Linha, 180, 15, tcDe2);
//    Juros := LerCampo(Linha, 195, 15, tcDe2);
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

end.

