{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreGNREW;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pgnreGNRE, pgnreConversao;

type
  TGNREW = class(TObject)
  private
    FGerador: TGerador;
    FGNRE: TGNRE;
    FVersao: TVersaoGNRE;
  public
    constructor Create(AOwner: TGNRE);
    destructor Destroy; override;

    function GerarXml: boolean;
    function GerarXml1: boolean;
    function GerarXml2: boolean;
    function ObterNomeArquivo: string;

    property Gerador: TGerador   read FGerador write FGerador;
    property GNRE: TGNRE         read FGNRE    write FGNRE;
    property Versao: TVersaoGNRE read FVersao  write FVersao;
  end;

implementation

const
  DSC_TIPOGNRE = 'Tipo de GNRE';

{ TGNREW }

constructor TGNREW.Create(AOwner: TGNRE);
begin
  FGNRE    := AOwner;
  FGerador := TGerador.Create;
end;

destructor TGNREW.Destroy;
begin
  FGerador.Free;

  inherited;
end;

function TGNREW.GerarXml: boolean;
begin
  if Versao = ve100 then
    Result := GerarXml1
  else
    Result := GerarXml2;
end;

function TGNREW.GerarXml1: boolean;
var
  i  : Integer;
  Doc: string;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';

//  Gerador.wGrupo('TDadosGNRE versao="1.00"');
  Gerador.wGrupo('TDadosGNRE');

  Gerador.wCampo(tcStr, '', 'c01_UfFavorecida  ', 002, 002, 1, GNRE.c01_UfFavorecida, DSC_UF + ' Favorecida');
  Gerador.wCampo(tcInt, '', 'c02_receita   ', 006, 006, 1, GNRE.c02_receita, '');
  if GNRE.c25_detalhamentoReceita > 0 then
    Gerador.wCampo(tcInt, '', 'c25_detalhamentoReceita   ', 006, 006, 1, GNRE.c25_detalhamentoReceita, '');

  if GNRE.c26_produto > 0 then
    Gerador.wCampo(tcInt, '', 'c26_produto   ', 001, 004, 1, GNRE.c26_produto, '');

  if GNRE.c27_tipoIdentificacaoEmitente > 0 then
    Gerador.wCampo(tcInt, '', 'c27_tipoIdentificacaoEmitente   ', 001, 001, 1, GNRE.c27_tipoIdentificacaoEmitente, '');

  if GNRE.c03_idContribuinteEmitente <> '' then
  begin
    Gerador.wGrupo('c03_idContribuinteEmitente');
    if GNRE.c27_tipoIdentificacaoEmitente = 1 then
    begin
      Doc := GNRE.c03_idContribuinteEmitente;
      Doc := StringReplace(Doc, '<CNPJ>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CNPJ>', '', [rfReplaceAll]);
      if not ValidarCNPJ(Doc) then
        Gerador.wAlerta('', 'CNPJ', DSC_CNPJ + ' Emitente', ERR_MSG_INVALIDO);

     // Gerador.wGrupo('CNPJ');
      Gerador.wCampo(tcStr, '', 'CNPJ   ', 014, 014, 1, GNRE.c03_idContribuinteEmitente, DSC_CNPJ + ' Emitente');
     // Gerador.wGrupo('/CNPJ');
    end
    else
    begin
      Doc := GNRE.c03_idContribuinteEmitente;
      Doc := StringReplace(Doc, '<CPF>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CPF>', '', [rfReplaceAll]);
      if not ValidarCPF(Doc) then
        Gerador.wAlerta('', 'CPF', DSC_CPF + ' Emitente', ERR_MSG_INVALIDO);

     // Gerador.wGrupo('CPF');
      Gerador.wCampo(tcStr, '', 'CPF   ', 011, 011, 1, GNRE.c03_idContribuinteEmitente, DSC_CPF + ' Emitente');
     // Gerador.wGrupo('/CPF');
    end;
    Gerador.wGrupo('/c03_idContribuinteEmitente');
  end;

  if GNRE.c28_tipoDocOrigem > 0 then
    Gerador.wCampo(tcInt, '', 'c28_tipoDocOrigem   ', 002, 002, 1, GNRE.c28_tipoDocOrigem, '');

  if GNRE.c04_docOrigem <> '' then
    Gerador.wCampo(tcStr, '', 'c04_docOrigem   ', 001, 018, 1, GNRE.c04_docOrigem, '');

  if GNRE.c06_valorPrincipal > 0 then
    Gerador.wCampo(tcDe2, '', 'c06_valorPrincipal   ', 001, 015, 1, GNRE.c06_valorPrincipal, '');

  if GNRE.c10_valorTotal > 0 then
    Gerador.wCampo(tcDe2, '', 'c10_valorTotal   ', 001, 015, 1, GNRE.c10_valorTotal, '');

  Gerador.wCampo(tcDat, '', 'c14_dataVencimento   ', 010, 010, 1, GNRE.c14_dataVencimento, '');

  if GNRE.c15_convenio <> '' then
    Gerador.wCampo(tcStr, '', 'c15_convenio   ', 001, 030, 1, GNRE.c15_convenio, '');

  if GNRE.c16_razaoSocialEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c16_razaoSocialEmitente   ', 001, 060, 1, GNRE.c16_razaoSocialEmitente, '');

  if GNRE.c17_inscricaoEstadualEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c17_inscricaoEstadualEmitente   ', 002, 016, 1, GNRE.c17_inscricaoEstadualEmitente, DSC_IE + ' Emitente');

  if GNRE.c18_enderecoEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c18_enderecoEmitente   ', 001, 060, 1, GNRE.c18_enderecoEmitente, '');

  if GNRE.c19_municipioEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c19_municipioEmitente   ', 001, 005, 1, GNRE.c19_municipioEmitente, DSC_CMUN + ' Emitente');

  if GNRE.c20_ufEnderecoEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c20_ufEnderecoEmitente   ', 002, 002, 1, GNRE.c20_ufEnderecoEmitente, DSC_UF + ' Emitente');

  if GNRE.c21_cepEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c21_cepEmitente   ', 008, 008, 1, GNRE.c21_cepEmitente, DSC_CEP + ' Emitente');

  if GNRE.c22_telefoneEmitente <> '' then
    Gerador.wCampo(tcStr, '', 'c22_telefoneEmitente   ', 006, 011, 1, GNRE.c22_telefoneEmitente, DSC_FONE + ' Emitente');

  if GNRE.c34_tipoIdentificacaoDestinatario > 0 then
    Gerador.wCampo(tcInt, '', 'c34_tipoIdentificacaoDestinatario   ', 001, 001, 1, GNRE.c34_tipoIdentificacaoDestinatario, '');

  if GNRE.c35_idContribuinteDestinatario <> '' then
  begin
    Gerador.wGrupo('c35_idContribuinteDestinatario');
    if GNRE.c34_tipoIdentificacaoDestinatario = 1 then
    begin
      Doc := GNRE.c35_idContribuinteDestinatario;
      Doc := StringReplace(Doc, '<CNPJ>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CNPJ>', '', [rfReplaceAll]);
      if not ValidarCNPJ(Doc) then
        Gerador.wAlerta('', 'CNPJ', DSC_CNPJ + ' Destinatário', ERR_MSG_INVALIDO);

     // Gerador.wGrupo('CNPJ');
      Gerador.wCampo(tcStr, '', 'CNPJ   ', 014, 014, 1, GNRE.c35_idContribuinteDestinatario, DSC_CNPJ + ' Destinatário');
      //Gerador.wGrupo('/CNPJ');
    end
    else
    begin
      Doc := GNRE.c35_idContribuinteDestinatario;
      Doc := StringReplace(Doc, '<CPF>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CPF>', '', [rfReplaceAll]);
      if not ValidarCPF(Doc) then
        Gerador.wAlerta('', 'CPF', DSC_CPF + ' Destinatário', ERR_MSG_INVALIDO);

     // Gerador.wGrupo('CPF');
      Gerador.wCampo(tcStr, '', 'CPF   ', 011, 011, 1, GNRE.c35_idContribuinteDestinatario, DSC_CPF + ' Destinatário');
      //Gerador.wGrupo('/CPF');
    end;
    Gerador.wGrupo('/c35_idContribuinteDestinatario');
  end;

  if GNRE.c36_inscricaoEstadualDestinatario <> '' then
    Gerador.wCampo(tcStr, '', 'c36_inscricaoEstadualDestinatario   ', 002, 016, 1, GNRE.c36_inscricaoEstadualDestinatario, DSC_IE + ' Destinatário');

  if GNRE.c37_razaoSocialDestinatario <> '' then
    Gerador.wCampo(tcStr, '', 'c37_razaoSocialDestinatario   ', 001, 060, 1, GNRE.c37_razaoSocialDestinatario, DSC_XNOME + ' Destinatário');

  if GNRE.c38_municipioDestinatario <> '' then
    Gerador.wCampo(tcStr, '', 'c38_municipioDestinatario   ', 001, 005, 1, GNRE.c38_municipioDestinatario, DSC_CMUN + ' Destinatário');

  Gerador.wCampo(tcDat, '', 'c33_dataPagamento   ', 010, 010, 0, GNRE.c33_dataPagamento, '');

  if (GNRE.referencia.periodo >= 0) or (GNRE.referencia.mes <> '') or (GNRE.referencia.ano > 0) or
    (GNRE.referencia.parcela > 0) then
  begin
    Gerador.wGrupo('c05_referencia');
    if GNRE.referencia.periodo >= 0 then
      Gerador.wCampo(tcInt, '', 'periodo   ', 001, 001, 1, GNRE.referencia.periodo, '');

    if GNRE.referencia.mes <> '' then
      Gerador.wCampo(tcInt, '', 'mes   ', 002, 002, 1, GNRE.referencia.mes, '');

    if GNRE.referencia.ano > 0 then
      Gerador.wCampo(tcInt, '', 'ano   ', 004, 004, 1, GNRE.referencia.ano, '');

    if GNRE.referencia.parcela > 0 then
      Gerador.wCampo(tcInt, '', 'parcela   ', 001, 003, 1, GNRE.referencia.parcela, '');
    Gerador.wGrupo('/c05_referencia');
  end;

  if GNRE.camposExtras.Count > 0 then
  begin
    Gerador.wGrupo('c39_camposExtras');
    for i := 0 to GNRE.camposExtras.Count - 1 do
    begin
      Gerador.wGrupo('campoExtra');
      Gerador.wCampo(tcInt, '', 'codigo   ', 001, 003, 1, GNRE.camposExtras.Items[i].CampoExtra.codigo, '');
      Gerador.wCampo(tcStr, '', 'tipo   ', 001, 001, 1, GNRE.camposExtras.Items[i].CampoExtra.tipo, '');
      Gerador.wCampo(tcStr, '', 'valor   ', 001, 100, 1, GNRE.camposExtras.Items[i].CampoExtra.valor, '');
      Gerador.wGrupo('/campoExtra');
    end;
    Gerador.wGrupo('/c39_camposExtras');
  end;

  if GNRE.c42_identificadorGuia <> '' then
    Gerador.wCampo(tcStr, '', 'c42_identificadorGuia   ', 001, 010, 1, GNRE.c42_identificadorGuia, '');
  Gerador.wGrupo('/TDadosGNRE');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TGNREW.GerarXml2: boolean;
var
  i  : Integer;
  Doc: string;
  LValorGNRE : Currency;
begin
  LValorGNRE := 0;
  
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('TDadosGNRE versao="2.00"');
//  Gerador.wGrupo('TDadosGNRE');

  Gerador.wCampo(tcStr, '', 'ufFavorecida', 2, 2, 1, GNRE.c01_UfFavorecida, DSC_UF + ' Favorecida');
  Gerador.wCampo(tcStr, '', 'tipoGnre    ', 1, 1, 1, TipoGNREToStr(GNRE.tipoGnre), DSC_TIPOGNRE);

  if GNRE.c03_idContribuinteEmitente <> '' then
  begin
    Gerador.wGrupo('contribuinteEmitente');
    Gerador.wGrupo('identificacao');

    if GNRE.c27_tipoIdentificacaoEmitente = 1 then
    begin
      Doc := GNRE.c03_idContribuinteEmitente;
      Doc := StringReplace(Doc, '<CNPJ>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CNPJ>', '', [rfReplaceAll]);
      if not ValidarCNPJ(Doc) then
        Gerador.wAlerta('', 'CNPJ', DSC_CNPJ + ' Emitente', ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '', 'CNPJ   ', 014, 014, 1, GNRE.c03_idContribuinteEmitente, DSC_CNPJ + ' Emitente');
    end
    else
    begin
      Doc := GNRE.c03_idContribuinteEmitente;
      Doc := StringReplace(Doc, '<CPF>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CPF>', '', [rfReplaceAll]);
      if not ValidarCPF(Doc) then
        Gerador.wAlerta('', 'CPF', DSC_CPF + ' Emitente', ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '', 'CPF   ', 011, 011, 1, GNRE.c03_idContribuinteEmitente, DSC_CPF + ' Emitente');
    end;

    Gerador.wCampo(tcStr, '', 'IE', 002, 016, 0, GNRE.c17_inscricaoEstadualEmitente, DSC_IE + ' Emitente');

    Gerador.wGrupo('/identificacao');

    Gerador.wCampo(tcStr, '', 'razaoSocial', 01, 60, 0, GNRE.c16_razaoSocialEmitente, '');
    Gerador.wCampo(tcStr, '', 'endereco   ', 01, 60, 0, GNRE.c18_enderecoEmitente, '');
    Gerador.wCampo(tcStr, '', 'municipio  ', 01, 05, 0, GNRE.c19_municipioEmitente, DSC_CMUN + ' Emitente');
    Gerador.wCampo(tcStr, '', 'uf         ', 02, 02, 0, GNRE.c20_ufEnderecoEmitente, DSC_UF + ' Emitente');
    Gerador.wCampo(tcStr, '', 'cep        ', 08, 08, 0, GNRE.c21_cepEmitente, DSC_CEP + ' Emitente');
    Gerador.wCampo(tcStr, '', 'telefone   ', 06, 11, 0, GNRE.c22_telefoneEmitente, DSC_FONE + ' Emitente');

    Gerador.wGrupo('/contribuinteEmitente');
  end;

  Gerador.wGrupo('itensGNRE');
  Gerador.wGrupo('item');

  Gerador.wCampo(tcInt, '', 'receita            ', 06, 06, 1, GNRE.c02_receita, '');
  Gerador.wCampo(tcInt, '', 'detalhamentoReceita', 06, 06, 0, GNRE.c25_detalhamentoReceita, '');

  if GNRE.c04_docOrigem <> '' then
    Gerador.wCampo(tcStr, '', 'documentoOrigem    ', 01, 18, 1, GNRE.c04_docOrigem, '',
                   True, 'tipo="' + FormatFloat('00', GNRE.c28_tipoDocOrigem) + '"');
  Gerador.wCampo(tcInt, '', 'produto            ', 01, 04, 0, GNRE.c26_produto, '');

  if (GNRE.referencia.periodo >= 0) or (GNRE.referencia.mes <> '') or
     (GNRE.referencia.ano > 0) or (GNRE.referencia.parcela > 0) then
  begin
    Gerador.wGrupo('referencia');

    if GNRE.referencia.periodo >= 0 then
      Gerador.wCampo(tcInt, '', 'periodo', 1, 1, 1, GNRE.referencia.periodo, '');

    Gerador.wCampo(tcStr, '', 'mes    ', 2, 2, 0, GNRE.referencia.mes, '');
    Gerador.wCampo(tcInt, '', 'ano    ', 4, 4, 0, GNRE.referencia.ano, '');
    Gerador.wCampo(tcInt, '', 'parcela', 1, 3, 0, GNRE.referencia.parcela, '');

    Gerador.wGrupo('/referencia');
  end;

  if GNRE.c14_dataVencimento > 0 then
    Gerador.wCampo(tcDat, '', 'dataVencimento', 10, 10, 1, GNRE.c14_dataVencimento, '');

{
11 - Valor Principal ICMS
12 - Valor Principal Fundo de Pobreza (FP)
21 - Valor Total ICMS
22 - Valor Total FP
31 - Valor Multa ICMS
32 - Valor Multa FP
41 - Valor Juros ICMS
42 - Valor Juros FP
51 - Valor Atualização Monetaria ICMS
52 - Valor Atualização Monetaria FP
}
  if GNRE.c06_valorPrincipal > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.c06_valorPrincipal, '',
                         True, 'tipo="11"');

  if GNRE.ValorFECP > 0 then
  begin
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.ValorFECP, '',
                         True, 'tipo="12"');
    LValorGNRE := GNRE.ValorFECP;
  end;

  if GNRE.c10_valorTotal > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.c10_valorTotal, '',
                         True, 'tipo="21"');

  if GNRE.TotalFECP > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.TotalFECP, '',
                         True, 'tipo="22"');

  if GNRE.MultaICMS > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.MultaICMS, '',
                         True, 'tipo="31"');

  if GNRE.MultaFECP > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.MultaFECP, '',
                         True, 'tipo="32"');

  if GNRE.JurosICMS > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.JurosICMS, '',
                         True, 'tipo="41"');

  if GNRE.JurosFECP > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.JurosFECP, '',
                         True, 'tipo="42"');

  if GNRE.AtualMonetICMS > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.AtualMonetICMS, '',
                         True, 'tipo="51"');

  if GNRE.AtualMonetFECP > 0 then
    Gerador.wCampo(tcDe2, '', 'valor', 01, 15, 1, GNRE.AtualMonetFECP, '',
                         True, 'tipo="52"');

  Gerador.wCampo(tcStr, '', 'convenio', 01, 30, 0, GNRE.c15_convenio, '');

  if GNRE.c35_idContribuinteDestinatario <> '' then
  begin
    Gerador.wGrupo('contribuinteDestinatario');
    Gerador.wGrupo('identificacao');

    if GNRE.c34_tipoIdentificacaoDestinatario = 1 then
    begin
      Doc := GNRE.c35_idContribuinteDestinatario;
      Doc := StringReplace(Doc, '<CNPJ>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CNPJ>', '', [rfReplaceAll]);
      if not ValidarCNPJ(Doc) then
        Gerador.wAlerta('', 'CNPJ', DSC_CNPJ + ' Destinatário', ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '', 'CNPJ', 14, 14, 1, GNRE.c35_idContribuinteDestinatario, DSC_CNPJ + ' Destinatário');
    end
    else
    begin
      Doc := GNRE.c35_idContribuinteDestinatario;
      Doc := StringReplace(Doc, '<CPF>', '', [rfReplaceAll]);
      Doc := StringReplace(Doc, '</CPF>', '', [rfReplaceAll]);
      if not ValidarCPF(Doc) then
        Gerador.wAlerta('', 'CPF', DSC_CPF + ' Destinatário', ERR_MSG_INVALIDO);

      Gerador.wCampo(tcStr, '', 'CPF', 11, 11, 1, GNRE.c35_idContribuinteDestinatario, DSC_CPF + ' Destinatário');
    end;

    Gerador.wCampo(tcStr, '', 'IE', 002, 016, 0, GNRE.c36_inscricaoEstadualDestinatario, DSC_IE + ' Destinatário');

    Gerador.wGrupo('/identificacao');

    Gerador.wCampo(tcStr, '', 'razaoSocial', 01, 60, 0, GNRE.c37_razaoSocialDestinatario, DSC_XNOME + ' Destinatário');
    Gerador.wCampo(tcStr, '', 'municipio  ', 01, 05, 0, GNRE.c38_municipioDestinatario, DSC_CMUN + ' Destinatário');

    Gerador.wGrupo('/contribuinteDestinatario');
  end;

  if GNRE.camposExtras.Count > 0 then
  begin
    Gerador.wGrupo('camposExtras');
    for i := 0 to GNRE.camposExtras.Count - 1 do
    begin
      Gerador.wGrupo('campoExtra');
      Gerador.wCampo(tcInt, '', 'codigo', 01, 003, 1, GNRE.camposExtras.Items[i].CampoExtra.codigo, '');
//      Gerador.wCampo(tcStr, '', 'tipo  ', 01, 001, 1, GNRE.camposExtras.Items[i].CampoExtra.tipo, '');
      Gerador.wCampo(tcStr, '', 'valor ', 01, 100, 1, GNRE.camposExtras.Items[i].CampoExtra.valor, '');
      Gerador.wGrupo('/campoExtra');
    end;
    Gerador.wGrupo('/camposExtras');
  end;

  if GNRE.numeroControle <> '' then
    Gerador.wCampo(tcStr, '', 'numeroControle', 0, 1, 0, GNRE.numeroControle, '');

  Gerador.wGrupo('/item');

  Gerador.wGrupo('/itensGNRE');

  if GNRE.c06_valorPrincipal > 0 then
    LValorGNRE := LValorGNRE + GNRE.c06_valorPrincipal
  else
  if GNRE.c10_valorTotal > 0 then
    LValorGNRE := LValorGNRE + GNRE.c10_valorTotal;

  if LValorGNRE > 0 then
     Gerador.wCampo(tcDe2, '', 'valorGNRE', 01, 15, 1, LValorGNRE, '');

  Gerador.wCampo(tcDat, '', 'dataPagamento    ', 10, 10, 0, GNRE.c33_dataPagamento, '');
  Gerador.wCampo(tcStr, '', 'identificadorGuia', 01, 10, 0, GNRE.c42_identificadorGuia, '');

  Gerador.wGrupo('/TDadosGNRE');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TGNREW.ObterNomeArquivo: string;
begin
  Result := '';
end;

end.
