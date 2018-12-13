{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSeR;

interface

uses
  SysUtils, Classes, Forms, DateUtils, Variants, IniFiles, Math, StrUtils,
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsNFSe, pnfsConversao,
  ACBrUtil;

type

 TLeitorOpcoes   = class;

 { TNFSeR }

 TNFSeR = class(TPersistent)
  private
    Nivel: Integer;

    FLeitor: TLeitor;
    FNFSe: TNFSe;
    FOpcoes: TLeitorOpcoes;
    FVersaoXML: String;
    FProvedor: TnfseProvedor;
    FTabServicosExt: Boolean;
    FProvedorConf: TnfseProvedor;
    FPathIniCidades: String;
    FVersaoNFSe: TVersaoNFSe;
    FLayoutXML: TLayoutXML;

    function LerRPS_ABRASF_V1: Boolean;
    function LerRPS_ABRASF_V2: Boolean;

    function LerRPS_ISSDSF: Boolean;
    function LerRPS_Equiplano: Boolean;
    function LerRps_EL: Boolean;
    function LerRps_Governa: Boolean;
    function LerRPS_Agili: Boolean;
    function LerRPS_SP: Boolean;

    function LerNFSe_ABRASF_V1: Boolean;
    function LerNFSe_ABRASF_V2: Boolean;

    function LerNFSe_ISSDSF: Boolean;
    function LerNFSe_Equiplano: Boolean;
    function LerNFSe_Infisc: Boolean;
    function LerNFSe_EL: Boolean;
    function LerNFSe_Governa: Boolean;
    function LerNFSe_CONAM: Boolean;
    function LerNFSe_Agili: Boolean;

    function LerNFSe_Infisc_V10: Boolean;
    function LerNFSe_Infisc_V11: Boolean;

    function LerNFSe_SP: Boolean;
    function LerNFSe_Smarapd: Boolean;
    function LerNFSe_IPM: Boolean;

    function LerRPS: Boolean;
    function LerNFSe: Boolean;

    function CodCidadeToProvedor(CodCidade: String): TNFSeProvedor;
    procedure SetxItemListaServico;
  public
    constructor Create(AOwner: TNFSe);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor             read FLeitor         write FLeitor;
    property NFSe: TNFSe                 read FNFSe           write FNFSe;
    property Opcoes: TLeitorOpcoes       read FOpcoes         write FOpcoes;
    property VersaoXML: String           read FVersaoXML      write FVersaoXML;
    property Provedor: TnfseProvedor     read FProvedor       write FProvedor;
    property ProvedorConf: TnfseProvedor read FProvedorConf   write FProvedorConf;
    property TabServicosExt: Boolean     read FTabServicosExt write FTabServicosExt;
    property PathIniCidades: String      read FPathIniCidades write FPathIniCidades;
    property VersaoNFSe: TVersaoNFSe     read FVersaoNFSe     write FVersaoNFSe;
    property LayoutXML: TLayoutXML       read FLayoutXML      write FLayoutXML;
  end;

 TLeitorOpcoes = class(TPersistent)
  private
    FPathArquivoMunicipios: String;
    FPathArquivoTabServicos: String;
  published
    property PathArquivoMunicipios: String  read FPathArquivoMunicipios  write FPathArquivoMunicipios;
    property PathArquivoTabServicos: String read FPathArquivoTabServicos write FPathArquivoTabServicos;
  end;

implementation

{ TNFSeR }

constructor TNFSeR.Create(AOwner: TNFSe);
begin
  FLeitor := TLeitor.Create;
  FNFSe   := AOwner;
  FOpcoes := TLeitorOpcoes.Create;
  FOpcoes.FPathArquivoMunicipios  := '';
  FOpcoes.FPathArquivoTabServicos := '';
end;

destructor TNFSeR.Destroy;
begin
  FLeitor.Free;
  FOpcoes.Free;

  inherited Destroy;
end;

function TNFSeR.CodCidadeToProvedor(CodCidade: String): TNFSeProvedor;
var
  Ok: Boolean;
  NomeArqParams: String;
  IniParams: TMemIniFile;
begin
  NomeArqParams := PathIniCidades + '\Cidades.ini';

  if not FileExists(NomeArqParams) then
    raise Exception.Create('Arquivo de Parâmetro não encontrado: ' +
      NomeArqParams);

  IniParams := TMemIniFile.Create(NomeArqParams);

  Result := StrToProvedor(Ok, IniParams.ReadString(CodCidade, 'Provedor', ''));

  IniParams.Free;
end;

procedure TNFSeR.SetxItemListaServico;
var
  Item: Integer;
begin
  NFSe.Servico.ItemListaServico := OnlyNumber(Leitor.rCampo(tcStr, 'ItemListaServico'));

  if NFSe.Servico.ItemListaServico = '' then
    NFSe.Servico.ItemListaServico := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoServico'));

  Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
  if Item < 100 then
    Item := Item * 100 + 1;

  NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);

  if not (FProvedor in [ProRJ, ProSisPMJP]) then
    NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                     Copy(NFSe.Servico.ItemListaServico, 3, 2);

  if TabServicosExt then
    NFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
  else
    NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));
end;

function TNFSeR.LerXml: Boolean;
begin
  if (Pos('<Nfse', Leitor.Arquivo) > 0) or (Pos('<Notas>', Leitor.Arquivo) > 0) or
     (Pos('<Nota>', Leitor.Arquivo) > 0) or (Pos('<NFS-e>', Leitor.Arquivo) > 0) or
     (Pos('<nfse', Leitor.Arquivo) > 0) or (Pos('NumNot', Leitor.Arquivo) > 0) or
     (Pos('<ConsultaNFSe>', Leitor.Arquivo) > 0) or (Pos('<Reg20Item>', Leitor.Arquivo) > 0) or
     (Pos('<CompNfse', Leitor.Arquivo) > 0) or (Pos('<NFe', Leitor.Arquivo) > 0) or
     (Pos('<notasFiscais>', Leitor.Arquivo) > 0) or (Pos('<nfeRpsNotaFiscal>', Leitor.Arquivo) > 0) or (Pos('<nfs', Leitor.Arquivo) > 0) then
    Result := LerNFSe
  else
    if (Pos('<Rps', Leitor.Arquivo) > 0) or (Pos('<rps', Leitor.Arquivo) > 0) or
       (Pos('<RPS', Leitor.Arquivo) > 0) then
      Result := LerRPS
  else
    if (Pos('<nfdok', Leitor.Arquivo) > 0) then
      Result := LerNFSe_Smarapd
    else
      Result := False;

  // Grupo da TAG <signature> ***************************************************
  Leitor.Grupo := Leitor.Arquivo;

  NFSe.Signature.URI             := Leitor.rAtributo('Reference URI=');
  NFSe.Signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
  NFSe.Signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
  NFSe.Signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
end;

////////////////////////////////////////////////////////////////////////////////
//  Funções especificas para ler o XML de um RPS                              //
////////////////////////////////////////////////////////////////////////////////

function TNFSeR.LerRPS: Boolean;
var
  CM: String;
begin
  Result := False;

  if FProvedor = proNenhum then
  begin
    if (Leitor.rExtrai(1, 'OrgaoGerador') <> '') then
    begin
      CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      FProvedor := CodCidadeToProvedor(CM);
    end;

    if FProvedor = proNenhum then
    begin
      if (Leitor.rExtrai(1, 'Servico') <> '') then
      begin
        CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
        FProvedor := CodCidadeToProvedor(CM);
      end;
    end;

    if FProvedor = proNenhum then
    begin
      if (Leitor.rExtrai(1, 'PrestadorServico') <> '') then
      begin
        CM := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoMunicipio'));
        if CM = '' then
          CM := Leitor.rCampo(tcStr, 'Cidade');
        FProvedor := CodCidadeToProvedor(CM);
      end;
    end;

    // ISSDSF
    if FProvedor = proNenhum then
    begin
      if (Leitor.rExtrai(1, 'Cabecalho') <> '') then
      begin
        CM := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CodCidade') );
        FProvedor := CodCidadeToProvedor(CM);
      end
    end;

    if FProvedor = proNenhum then
      FProvedor := FProvedorConf;
  end;

  VersaoNFSe := ProvedorToVersaoNFSe(FProvedor);
  LayoutXML := ProvedorToLayoutXML(FProvedor);

  if (Leitor.rExtrai(1, 'Rps') <> '') or (Leitor.rExtrai(1, 'RPS') <> '') or
     (Leitor.rExtrai(1, 'LoteRps') <> '') then
  begin
    case LayoutXML of
      loABRASFv1:    Result := LerRPS_ABRASF_V1;
      loABRASFv2:    Result := LerRPS_ABRASF_V2;
      loEGoverneISS: Result := False; // Falta implementar
      loEL:          Result := LerRps_EL;
      loEquiplano:   Result := LerRPS_Equiplano;
      loGoverna:     Result := LerRps_Governa;
      loInfisc:      Result := False; // Falta implementar
      loISSDSF:      Result := LerRPS_ISSDSF;
      loAgili:       Result := LerRPS_Agili;
      loSP:          Result := LerRPS_SP;
      loSMARAPD:     Result := LerNFSe_Smarapd;
    else
      Result := False;
    end;
  end;
end;

function TNFSeR.LerRPS_ABRASF_V1: Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  if (Leitor.rExtrai(2, 'InfRps') <> '') or (Leitor.rExtrai(1, 'Rps') <> '') then
  begin
    NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');

    if (Leitor.rExtrai(1, 'InfRps') <> '') then
      NFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');

    NFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
    NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
    NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
    NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));
    NFSe.Status                   := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));
    NFSe.OutrasInformacoes        := Leitor.rCampo(tcStr, 'OutrasInformacoes');

    if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '') or
       (Leitor.rExtrai(2, 'IdentificacaoRps') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;

    if (Leitor.rExtrai(3, 'RpsSubstituido') <> '') or
       (Leitor.rExtrai(2, 'RpsSubstituido') <> '') then
    begin
      NFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    end;

    if (Leitor.rExtrai(3, 'Servico') <> '') or
       (Leitor.rExtrai(2, 'Servico') <> '') then
    begin
      NFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
      NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
      NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
      NFSe.Servico.Descricao                 := '';

      NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'MunicipioPrestacaoServico');
      if NFSe.Servico.CodigoMunicipio = '' then
        NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

      SetxItemListaServico;

      if length(NFSe.Servico.CodigoMunicipio) < 7 then
        NFSe.Servico.CodigoMunicipio := Copy(NFSe.Servico.CodigoMunicipio, 1, 2) +
            FormatFloat('00000', StrToIntDef(Copy(NFSe.Servico.CodigoMunicipio, 3, 5), 0));

      if (Leitor.rExtrai(4, 'Valores') <> '') or
         (Leitor.rExtrai(3, 'Valores') <> '') then
      begin
        NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
        NFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
        NFSe.Servico.Valores.ValorTotalRecebido     := Leitor.rCampo(tcDe2, 'ValorTotalRecebido');
        NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
        NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
        NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
        NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
        NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
        NFSe.Servico.Valores.IssRetido              := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
        NFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
        NFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
        NFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
        NFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
        NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
        NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
        NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
        NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');
      end;

      //Provedor SimplISS permite varios itens servico
      if FProvedor = proSimplISS then
      begin
        i := 1;
        while (Leitor.rExtrai(4, 'ItensServico', 'ItensServico', i) <> '') do
        begin
          with NFSe.Servico.ItemServico.Add do
          begin
            Descricao := Leitor.rCampo(tcStr, 'Descricao');
//            Quantidade := Leitor.rCampo(tcInt, 'Quantidade');
            Quantidade := Leitor.rCampo(tcDe2, 'Quantidade');
            ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
          end;
          inc(i);
        end;
      end;
    end; // fim Servico

    if (Leitor.rExtrai(3, 'Prestador') <> '') or
       (Leitor.rExtrai(2, 'Prestador') <> '') then
    begin
      NFSe.Prestador.Cnpj               := Leitor.rCampo(tcStr, 'Cnpj');
      NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
    end; // fim Prestador

    if (Leitor.rExtrai(3, 'Tomador') <> '') or (Leitor.rExtrai(3, 'TomadorServico') <> '') or
       (Leitor.rExtrai(2, 'Tomador') <> '') or (Leitor.rExtrai(2, 'TomadorServico') <> '') then
    begin
      NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := Leitor.rCampo(tcStr, 'InscricaoEstadual');

      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>' then
        NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);

      NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
      NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
      NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      if NFSe.Tomador.Endereco.CodigoMunicipio = '' then
        NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');

      NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
      if NFSe.Tomador.Endereco.UF = '' then
        NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

      NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

      if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7 then
        NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
             FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

      if NFSe.Tomador.Endereco.UF = '' then
        NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

      NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

      if Leitor.rExtrai(4, 'IdentificacaoTomador') <> '' then
      begin
        NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

        if Leitor.rExtrai(5, 'CpfCnpj') <> '' then
        begin
          if Leitor.rCampo(tcStr, 'Cpf') <> '' then
            NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
          else
            NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end;

      if Leitor.rExtrai(4, 'Contato') <> '' then
      begin
        NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
        NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;

    end; // fim Tomador

    if Leitor.rExtrai(3, 'IntermediarioServico') <> '' then
    begin
      NFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
      NFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      if Leitor.rExtrai(4, 'CpfCnpj') <> '' then
      begin
        if Leitor.rCampo(tcStr, 'Cpf')<>'' then
          NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
        else
          NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;
    end;

    if Leitor.rExtrai(3, 'ConstrucaoCivil') <> '' then
    begin
      NFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
      NFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
    end;
  end; // fim InfRps

  Result := True;
end;

function TNFSeR.LerRPS_ABRASF_V2: Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  // Para o provedor ISSDigital
  if (Leitor.rExtrai(2, 'ValoresServico') <> '') then
  begin
    NFSe.Servico.Valores.ValorServicos    := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
    NFSe.Servico.Valores.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
  end;

  // Para o provedor ISSDigital
  if (Leitor.rExtrai(2, 'ListaServicos') <> '') then
  begin
    NFSe.Servico.Valores.IssRetido   := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
    NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));

    SetxItemListaServico;

    //NFSe.Servico.Discriminacao       := Leitor.rCampo(tcStr, 'Discriminacao');
    NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
    NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
    NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');

    NFSe.Servico.Valores.Aliquota    := Leitor.rCampo(tcDe3, 'Aliquota');

    //Se não me engano o maximo de servicos é 10...não?
    for I := 1 to 10 do
    begin
      if (Leitor.rExtrai(2, 'Servico', 'Servico', i) <> '') then
      begin
        with NFSe.Servico.ItemServico.Add do
        begin
          Descricao := Leitor.rCampo(tcStr, 'Discriminacao');

          if (Leitor.rExtrai(3, 'Valores') <> '') then
          begin
            ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
            ValorDeducoes := Leitor.rCampo(tcDe2, 'ValorDeducoes');
            ValorIss      := Leitor.rCampo(tcDe2, 'ValorIss');
            Aliquota      := Leitor.rCampo(tcDe3, 'Aliquota');
            BaseCalculo   := Leitor.rCampo(tcDe2, 'BaseCalculo');
          end;
        end;
      end
      else
        Break;
    end;
  end; // fim lista serviço

  if (Leitor.rExtrai(2, 'InfDeclaracaoPrestacaoServico') <> '') or
     (Leitor.rExtrai(1, 'InfDeclaracaoPrestacaoServico') <> '') then
  begin
    if FProvedor = ProTecnos then
      NFSe.Competencia := DateTimeToStr(StrToFloatDef(Leitor.rCampo(tcDatHor, 'Competencia'), 0))
    else
      NFSe.Competencia := Leitor.rCampo(tcStr, 'Competencia');

    NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
    NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
    NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));
    NFSe.Producao                 := StrToSimNao(ok, Leitor.rCampo(tcStr, 'Producao'));

    if (Leitor.rExtrai(3, 'Rps') <> '') or (Leitor.rExtrai(2, 'Rps') <> '') then
    begin
      NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
      NFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao');
      NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

      if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '') then
      begin
        NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
        NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
        NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
        NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
      end;
    end;

    if (Leitor.rExtrai(3, 'Servico') <> '') or (Leitor.rExtrai(2, 'Servico') <> '') then
    begin
      NFSe.Servico.Valores.IssRetido   := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
      NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
      NFSe.Servico.CodigoCnae          := Leitor.rCampo(tcStr, 'CodigoCnae');

      SetxItemListaServico;

      NFSe.Servico.Discriminacao       := Leitor.rCampo(tcStr, 'Discriminacao');
      NFSe.Servico.Descricao           := '';
      NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
      if (FProvedor = proABAse) then
        NFSe.Servico.ExigibilidadeISS          :=  StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'Exigibilidade'))
      else
        NFSe.Servico.ExigibilidadeISS          := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
      //NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
      NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');

      // Provedor Goiania
      NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');

      if (Leitor.rExtrai(4, 'Valores') <> '') or (Leitor.rExtrai(3, 'Valores') <> '') then
      begin
        NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
        NFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
        NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
        NFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
        NFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');
        NFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');

        // Provedor Goiania
        NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
        NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
        NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
        NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
        NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe3, 'DescontoIncondicionado');
        NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');

        if (FProvedor in [proISSe, proVersaTecnologia, proNEAInformatica, proFiorilli, proPronimv2, proEReceita]) then
        begin
          if NFSe.Servico.Valores.IssRetido = stRetencao then
            NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIss')
          else
            NFSe.Servico.Valores.ValorIssRetido := 0;
        end
        else
          NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIssRetido');

        if NFSe.Servico.Valores.ValorLiquidoNfse = 0 then
          NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                                   NFSe.Servico.Valores.DescontoIncondicionado -
                                                   NFSe.Servico.Valores.DescontoCondicionado -
                                                   // Retenções Federais
                                                   NFSe.Servico.Valores.ValorPis -
                                                   NFSe.Servico.Valores.ValorCofins -
                                                   NFSe.Servico.Valores.ValorIr -
                                                   NFSe.Servico.Valores.ValorInss -
                                                   NFSe.Servico.Valores.ValorCsll -

                                                   NFSe.Servico.Valores.OutrasRetencoes -
                                                   NFSe.Servico.Valores.ValorIssRetido;

        if NFSe.Servico.Valores.BaseCalculo = 0 then
          NFSe.Servico.Valores.BaseCalculo := NFSe.Servico.Valores.ValorServicos -
                                              NFSe.Servico.Valores.ValorDeducoes -
                                              NFSe.Servico.Valores.DescontoIncondicionado;

//        if NFSe.Servico.Valores.ValorIss = 0 then
//          NFSe.Servico.Valores.ValorIss := (NFSe.Servico.Valores.BaseCalculo * NFSe.Servico.Valores.Aliquota)/100;

      end;
    end; // fim serviço

    if (Leitor.rExtrai(3, 'Prestador') <> '') or (Leitor.rExtrai(2, 'Prestador') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      NFSe.Prestador.InscricaoMunicipal := NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

      if (VersaoNFSe = ve100) or (FProvedor = proDigifred) then
      begin
        if (Leitor.rExtrai(4, 'CpfCnpj') <> '') or (Leitor.rExtrai(3, 'CpfCnpj') <> '') then
        begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
          if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
            NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end
      else
      begin
        NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;

      NFSe.Prestador.Cnpj := NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
    end; // fim Prestador

   if (Leitor.rExtrai(3, 'Tomador') <> '') or (Leitor.rExtrai(3, 'TomadorServico') <> '') or
      (Leitor.rExtrai(2, 'Tomador') <> '') or (Leitor.rExtrai(2, 'TomadorServico') <> '')
    then
    begin
      NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');

      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>' then
        NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);

      NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
      NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
      NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      if NFSe.Tomador.Endereco.CodigoMunicipio = '' then
        NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
      if NFSe.Tomador.Endereco.CodigoMunicipio = '' then
        NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipioIBGE');

      NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
      if NFSe.Tomador.Endereco.UF = '' then
        NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

      NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

      if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7 then
        NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
          FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

      if NFSe.Tomador.Endereco.UF = '' then
        NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

      NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

      NFSe.Tomador.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');

      if (Leitor.rExtrai(4, 'IdentificacaoTomador') <> '') or (Leitor.rExtrai(3, 'IdentificacaoTomador') <> '') then
      begin
        NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

        if (Leitor.rExtrai(5, 'CpfCnpj') <> '') or (Leitor.rExtrai(4, 'CpfCnpj') <> '') then
        begin
          if Leitor.rCampo(tcStr, 'Cpf')<>'' then
            NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
          else
            NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
      end;

      if (Leitor.rExtrai(4, 'Contato') <> '') or (Leitor.rExtrai(3, 'Contato') <> '') then
      begin
        NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
        NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;

    end; // fim Tomador
  end; // fim InfDeclaracaoPrestacaoServico

  Result := True;
end;

function TNFSeR.LerRPS_Agili: Boolean;
var
  item, i: Integer;
  ok: Boolean;
  itemServico: TItemServicoCollectionItem;
  codCNAE: String;
  codLCServ: String;

  function _StrToSimNao(out ok: boolean; const s: String): TnfseSimNao;
  begin
    result := StrToEnumerado(ok, s,
                             ['1','0'],
                             [snSim, snNao]);
  end;

  function _StrToResponsavelRetencao(out ok: boolean; const s: String): TnfseResponsavelRetencao;
  begin
    result := StrToEnumerado(ok, s,
                             ['-1', '-2', '-3'],
                             [ptTomador, rtPrestador, rtPrestador]);
  end;

  function _StrToRegimeEspecialTributacao(out ok: boolean; const s: String): TnfseRegimeEspecialTributacao;
  begin
    // -7 Microempresario individual MEI optante pelo SIMEI
    result := StrToEnumerado(ok, s,
                            ['-1','-2','-4','-5','-6'],
                            [retNenhum, retEstimativa, retCooperativa,
                             retMicroempresarioIndividual, retMicroempresarioEmpresaPP
                            ]);
  end;

  function _StrToExigibilidadeISS(out ok: boolean; const s: String): TnfseExigibilidadeISS;
  begin
    // -8 Fixo
    result := StrToEnumerado(ok, s,
                            ['-1','-2','-3','-4','-5','-6','-7'],
                             [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
                              exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
  end;

  function _StrToTipoRPS(out ok: boolean; const s: String): TnfseTipoRPS;
  begin
    result := StrToEnumerado(ok, s,
                             ['-2','-4','-5'],
                             [trRPS, trNFConjugada, trCupom]);
  end;

begin
  codLCServ := '';
  CodCNAE := '';

  if (Leitor.rExtrai(1, 'ListaServico') <> '') then
  begin
    i := 1;
    while (Leitor.rExtrai(2, 'DadosServico', '', i) <> '') do
    begin
      itemServico := NFSe.Servico.ItemServico.Add;
      itemServico.Descricao := Leitor.rCampo(tcStr, 'Discriminacao');
      itemServico.Discriminacao := itemServico.Descricao;
      itemServico.ValorServicos := Leitor.rCampo(tcDe2, 'ValorServico');
      itemServico.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'ValorDesconto');
      itemServico.Quantidade := Leitor.rCampo(tcDe6, 'Quantidade');

      if VersaoNFSe = ve100 then
        codCNAE := Leitor.rCampo(tcStr, 'CodigoCnae');

      itemServico.CodLCServ := Leitor.rCampo(tcStr, 'ItemLei116');

      Item := StrToIntDef(OnlyNumber(itemServico.CodLCServ), 0);
      if Item < 100 then
        Item := Item * 100 + 1;
      itemServico.CodLCServ := FormatFloat('0000', Item);
      itemServico.CodLCServ := Copy(itemServico.CodLCServ, 1, 2) + '.' + Copy(itemServico.CodLCServ, 3, 2);

      if codLCServ = '' then
        codLCServ := itemServico.CodLCServ;

      i := i + 1;
    end;
  end; // fim lista serviço

  if VersaoNFSe = ve100 then
  begin
    if Leitor.rExtrai(1, 'ResponsavelISSQN') <> '' then
      NFSe.Servico.ResponsavelRetencao := _StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'Codigo'));

    if Leitor.rExtrai(1, 'RegimeEspecialTributacao') <> '' then
      NFSe.RegimeEspecialTributacao := _StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'Codigo'));

    if Leitor.rExtrai(1, 'ExigibilidadeISSQN') <> '' then
      NFSe.Servico.ExigibilidadeISS := _StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'Codigo'));

    if Leitor.rExtrai(1, 'MunicipioIncidencia') <> '' then
    begin
      NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcInt, 'CodigoMunicipioIBGE');
      NFSe.Servico.CodigoPais          := 1058;
      NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'CodigoMunicipioIBGE');
    end;
  end;

  if (Leitor.rExtrai(2, 'InfDeclaracaoPrestacaoServico') <> '') or
     (Leitor.rExtrai(1, 'InfDeclaracaoPrestacaoServico') <> '') then
  begin
    //NFSe.Competencia := Leitor.rCampo(tcStr, 'Competencia');
    NFSe.OptanteSimplesNacional := _StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));

    if VersaoNFSe = ve100 then
    begin
      NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida');
      // OptanteMEISimei
      NFSe.IncentivadorCultural := _StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));
    end
    else
    begin
      NFSe.RegimeEspecialTributacao    := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
      NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
      NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeIss'));
      NFSe.Servico.CodigoMunicipio     := Leitor.rCampo(tcInt, 'MunicipioIncidencia');
      NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');
    end;

    NFSe.Producao := _StrToSimNao(ok, Leitor.rCampo(tcStr, 'Producao'));
    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoAtividadeEconomica');
    NFSe.Servico.CodigoCnae := codCNAE;
    NFSe.Servico.ItemListaServico := codLCServ;

    if TabServicosExt then
      NFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
    else
      NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

    if VersaoNFSe = ve100 then
    begin
      NFSe.Servico.NumeroProcesso := Leitor.rCampo(tcStr, 'BeneficioProcesso');
      NFSe.Servico.Valores.ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
      NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'ValorDescontos');
      NFSe.Servico.Valores.ValorPis := Leitor.rCampo(tcDe2, 'ValorPis');
      NFSe.Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'ValorCofins');
      NFSe.Servico.Valores.ValorInss := Leitor.rCampo(tcDe2, 'ValorInss');
      NFSe.Servico.Valores.ValorIr := Leitor.rCampo(tcDe2, 'ValorIrrf');
      NFSe.Servico.Valores.ValorCsll := Leitor.rCampo(tcDe2, 'ValorCsll');
      NFSe.Servico.Valores.valorOutrasRetencoes := Leitor.rCampo(tcDe2, 'ValorOutrasRetencoes');
      NFSe.Servico.Valores.BaseCalculo := Leitor.rCampo(tcDe2, 'ValorBaseCalculoISSQN');
      NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'AliquotaISSQN');
      NFSe.Servico.Valores.ValorIss := Leitor.rCampo(tcDe3, 'ValorISSQNCalculado');
      case _StrToSimNao(ok, Leitor.rCampo(tcStr, 'ISSQNRetido')) of
        snSim: NFSe.Servico.Valores.ValorIssRetido := NFSe.Servico.Valores.ValorIss;
        snNao: NFSe.Servico.Valores.ValorIssRetido := 0;
      end;
      NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquido');
      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'Observacao');
    end
    else
    begin
      NFSe.Servico.NumeroProcesso := Leitor.rCampo(tcStr, 'NumeroProcesso');
      NFSe.Servico.Valores.ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
      NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'ValorDescontos');
      NFSe.Servico.Valores.ValorPis := Leitor.rCampo(tcDe2, 'ValorPis');
      NFSe.Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'ValorCofins');
      NFSe.Servico.Valores.ValorInss := Leitor.rCampo(tcDe2, 'ValorInss');
      NFSe.Servico.Valores.ValorIr := Leitor.rCampo(tcDe2, 'ValorIr');
      NFSe.Servico.Valores.ValorCsll := Leitor.rCampo(tcDe2, 'ValorCsll');
      NFSe.Servico.Valores.valorOutrasRetencoes := Leitor.rCampo(tcDe2, 'ValorOutrasRetencoes');
      NFSe.Servico.Valores.BaseCalculo := Leitor.rCampo(tcDe2, 'ValorBaseCalculoIss');
      NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'Aliquota');
      NFSe.Servico.Valores.ValorIss := Leitor.rCampo(tcDe3, 'ValorIss');
      case _StrToSimNao(ok, Leitor.rCampo(tcStr, 'IssRetido')) of
        snSim: NFSe.Servico.Valores.ValorIssRetido := NFSe.Servico.Valores.ValorIss;
        snNao: NFSe.Servico.Valores.ValorIssRetido := 0;
      end;
      NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquido');
    end;

    if (Leitor.rExtrai(3, 'Rps') <> '') or (Leitor.rExtrai(2, 'Rps') <> '') then
    begin
      NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
      NFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao');

      if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '') then
      begin
        NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
        NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');

        if VersaoNFSe = ve100 then
          NFSe.IdentificacaoRps.Tipo := _StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'))
        else
          NFSe.IdentificacaoRps.Tipo := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));

        NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
      end;
    end;

    if (Leitor.rExtrai(3, 'IdentificacaoPrestador') <> '') or (Leitor.rExtrai(2, 'IdentificacaoPrestador') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      NFSe.Prestador.InscricaoMunicipal := NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
      NFSe.PrestadorServico.IdentificacaoPrestador.ChaveAcesso := Leitor.rCampo(tcStr, 'ChaveDigital');
      NFSe.Prestador.ChaveAcesso := NFSe.PrestadorServico.IdentificacaoPrestador.ChaveAcesso;
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
      NFSe.Prestador.Cnpj := NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
    end; // fim Prestador

   if (Leitor.rExtrai(3, 'DadosTomador') <> '') or (Leitor.rExtrai(2, 'DadosTomador') <> '') then
   begin
     NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
     NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');

     NFSe.Tomador.Endereco.TipoLogradouro := Leitor.rCampo(tcStr, 'TipoLogradouro');
     NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Logradouro');
     NFSe.Tomador.Endereco.Numero := Leitor.rCampo(tcStr, 'Numero');
     NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
     NFSe.Tomador.Endereco.Bairro := Leitor.rCampo(tcStr, 'Bairro');
     NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipioIBGE');
     NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
     NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

     if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7
      then NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
        FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

     if NFSe.Tomador.Endereco.UF = ''
      then NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

     if (Leitor.rExtrai(4, 'IdentificacaoTomador') <> '') or (Leitor.rExtrai(3, 'IdentificacaoTomador') <> '') then
     begin
       NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

       if (Leitor.rExtrai(5, 'CpfCnpj') <> '') or (Leitor.rExtrai(4, 'CpfCnpj') <> '') then
       begin
         if Leitor.rCampo(tcStr, 'Cpf')<>''
          then NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
          else NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
       end;
     end;

     if (Leitor.rExtrai(4, 'Contato') <> '') or (Leitor.rExtrai(3, 'Contato') <> '') then
     begin
       NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
       NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
     end;

    end; // fim Tomador
  end; // fim InfDeclaracaoPrestacaoServico

  Result := True;
end;

function TNFSeR.LerRPS_ISSDSF: Boolean;
var
  item: Integer;
  ok  : Boolean;
  sOperacao, sTributacao: String;
begin
  VersaoNFSe := ve100; // para este provedor usar padrão "1".
  if (Leitor.rExtrai(1, 'Cabecalho') <> '') then
  begin
   	NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'CPFCNPJRemetente');
   	NFSe.Prestador.Cnpj                               := Leitor.rCampo(tcStr, 'CPFCNPJRemetente');
   	NFSe.PrestadorServico.RazaoSocial                 := Leitor.rCampo(tcStr, 'RazaoSocialRemetente');
   	NFSe.PrestadorServico.Endereco.CodigoMunicipio    := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CodCidade') );
  end;

  if (Leitor.rExtrai(1, 'RPS') <> '') then
  begin
    NFSe.DataEmissaoRPS := Leitor.rCampo(tcDatHor, 'DataEmissaoRPS');
    NFSe.Status         := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'Status'),['N','C'],[srNormal, srCancelado]);

    NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRPS');
    NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'SerieRPS');
    NFSe.IdentificacaoRps.Tipo   := trRPS;//StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero);// + NFSe.IdentificacaoRps.Serie;
    NFSe.SeriePrestacao          := Leitor.rCampo(tcStr, 'SeriePrestacao');

   	NFSe.Prestador.InscricaoMunicipal     := Leitor.rCampo(tcStr, 'InscricaoMunicipalPrestador');
    NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'DDDPrestador') + Leitor.rCampo(tcStr, 'TelefonePrestador');

   	NFSe.Tomador.RazaoSocial              := Leitor.rCampo(tcStr, 'RazaoSocialTomador');
    NFSe.Tomador.Endereco.TipoLogradouro  := Leitor.rCampo(tcStr, 'TipoLogradouroTomador');
    NFSe.Tomador.Endereco.Endereco        := Leitor.rCampo(tcStr, 'LogradouroTomador');
    NFSe.Tomador.Endereco.Numero          := Leitor.rCampo(tcStr, 'NumeroEnderecoTomador');
    NFSe.Tomador.Endereco.Complemento     := Leitor.rCampo(tcStr, 'ComplementoEnderecoTomador');
    NFSe.Tomador.Endereco.TipoBairro      := Leitor.rCampo(tcStr, 'TipoBairroTomador');
    NFSe.Tomador.Endereco.Bairro          := Leitor.rCampo(tcStr, 'BairroTomador');
    NFSe.Tomador.Endereco.CEP             := Leitor.rCampo(tcStr, 'CEPTomador');
   	NFSe.Tomador.Endereco.CodigoMunicipio := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CidadeTomador')) ;
    NFSe.Tomador.Endereco.xMunicipio      := Leitor.rCampo(tcStr, 'CidadeTomadorDescricao');
   	NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'Uf');
   	NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipalTomador');
   	NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'CPFCNPJTomador');
    NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro := Leitor.rCampo(tcStr, 'DocTomadorEstrangeiro');
    NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'EmailTomador');
    NFSe.Tomador.Contato.Telefone          := Leitor.rCampo(tcStr, 'DDDTomador') + Leitor.rCampo(tcStr, 'TelefoneTomador');
    NFSe.Servico.CodigoCnae := Leitor.rCampo(tcStr, 'CodigoAtividade');
    NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'AliquotaAtividade');

    NFSe.Servico.Valores.IssRetido := StrToEnumerado( ok, Leitor.rCampo(tcStr, 'TipoRecolhimento'),
                                                      ['A','R'], [ stNormal, stRetencao{, stSubstituicao}]);

    NFSe.Servico.CodigoMunicipio := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'MunicipioPrestacao'));

    sOperacao   := AnsiUpperCase(Leitor.rCampo(tcStr, 'Operacao'));
    sTributacao := AnsiUpperCase(Leitor.rCampo(tcStr, 'Tributacao'));
    NFSe.TipoRecolhimento := AnsiUpperCase(Leitor.rCampo(tcStr, 'TipoRecolhimento')); 

    if sOperacao[1] in ['A', 'B'] then
    begin
      if (sOperacao = 'A') and (sTributacao = 'N') then
        NFSe.NaturezaOperacao := no7
      else
        if sTributacao = 'G' then
          NFSe.NaturezaOperacao := no2
        else
          if sTributacao = 'T' then
            NFSe.NaturezaOperacao := no1;
    end
    else
      if (sOperacao = 'C') and (sTributacao = 'C') then
      begin
        NFSe.NaturezaOperacao := no3;
      end
      else
        if (sOperacao = 'C') and (sTributacao = 'F') then
        begin
          NFSe.NaturezaOperacao := no4;
        end;

    NFSe.Servico.Operacao := StrToOperacao(Ok, sOperacao);
    NFSe.Servico.Tributacao := StrToTributacao(Ok, sTributacao);

    NFSe.NaturezaOperacao := StrToEnumerado( ok,sTributacao, ['T','K'], [ NFSe.NaturezaOperacao, no5 ]);

    NFSe.OptanteSimplesNacional := StrToEnumerado( ok,sTributacao, ['T','H'], [ snNao, snSim ]);

    NFSe.DeducaoMateriais := StrToEnumerado( ok,sOperacao, ['A','B'], [ snNao, snSim ]);

    NFse.RegimeEspecialTributacao := StrToEnumerado( ok,sTributacao, ['T','M'], [ retNenhum, retMicroempresarioIndividual ]);

    //NFSe.Servico.Valores.ValorDeducoes          :=
    NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPIS');
    NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCOFINS');
    NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorINSS');
    NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIR');
    NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCSLL');
    NFSe.Servico.Valores.AliquotaPIS            := Leitor.rCampo(tcDe2, 'AliquotaPIS');
    NFSe.Servico.Valores.AliquotaCOFINS         := Leitor.rCampo(tcDe2, 'AliquotaCOFINS');
    NFSe.Servico.Valores.AliquotaINSS           := Leitor.rCampo(tcDe2, 'AliquotaINSS');
    NFSe.Servico.Valores.AliquotaIR             := Leitor.rCampo(tcDe2, 'AliquotaIR');
    NFSe.Servico.Valores.AliquotaCSLL           := Leitor.rCampo(tcDe2, 'AliquotaCSLL');

    NFSE.OutrasInformacoes := Leitor.rCampo(tcStr, 'DescricaoRPS');
    NFSE.MotivoCancelamento := Leitor.rCampo(tcStr, 'MotCancelamento');
    NFSE.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'CpfCnpjIntermediario');

    if (Leitor.rExtrai(1, 'Itens') <> '') then
    begin
      Item := 0 ;
      while (Leitor.rExtrai(1, 'Item', '', Item + 1) <> '') do
      begin
        FNfse.Servico.ItemServico.Add;
        FNfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'DiscriminacaoServico');
        FNfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe2, 'Quantidade');
        FNfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
        FNfse.Servico.ItemServico[Item].ValorTotal    := Leitor.rCampo(tcDe2, 'ValorTotal');
        FNfse.Servico.ItemServico[Item].Tributavel    := StrToEnumerado( ok,Leitor.rCampo(tcStr, 'Tributavel'), ['N','S'], [ snNao, snSim ]);
        FNfse.Servico.Valores.ValorServicos           := (FNfse.Servico.Valores.ValorServicos + FNfse.Servico.ItemServico[Item].ValorTotal);
        inc(Item);
      end;
    end;

//      FNfse.Servico.Valores.ValorIss                          := (FNfse.Servico.Valores.ValorServicos * NFSe.Servico.Valores.Aliquota)/100;
    FNFSe.Servico.Valores.ValorLiquidoNfse := (FNfse.Servico.Valores.ValorServicos -
                                              (FNfse.Servico.Valores.ValorDeducoes +
                                               FNfse.Servico.Valores.DescontoCondicionado+
                                               FNfse.Servico.Valores.DescontoIncondicionado+
                                               FNFSe.Servico.Valores.ValorIssRetido));
    FNfse.Servico.Valores.BaseCalculo      := NFSe.Servico.Valores.ValorLiquidoNfse;
  end; // fim Rps

  Result := True;
end;

function TNFSeR.LerRPS_SP: Boolean;
var
//  item: Integer;
  ok  : Boolean;
//  sOperacao, sTributacao: String;
begin
  if (Leitor.rExtrai(1, 'RPS') <> '') then
  begin
    NFSe.DataEmissaoRPS    := Leitor.rCampo(tcDat, 'DataEmissao');
    NFSe.Status            := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'StatusRPS'),['N','C'],[srNormal, srCancelado]);
    NFSe.TipoTributacaoRPS := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'TributacaoRPS'),['T','F', 'A', 'B', 'M', 'N', 'X', 'V', 'P'],[ttTribnoMun, ttTribforaMun, ttTribnoMunIsento, ttTribforaMunIsento, ttTribnoMunImune, ttTribforaMunImune, ttTribnoMunSuspensa, ttTribforaMunSuspensa, ttExpServicos]);

    NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoServico');

    NFSe.Servico.Valores.ValorServicos   := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.Servico.Valores.ValorDeducoes   := Leitor.rCampo(tcDe2, 'ValorDeducoes');
    NFSe.Servico.Valores.Aliquota        := Leitor.rCampo(tcDe3, 'AliquotaServicos');
    NFSe.Servico.Valores.IssRetido       := StrToEnumerado( ok, Leitor.rCampo(tcStr, 'ISSRetido'), ['false','true'], [ stNormal, stRetencao]);
    NFSe.Servico.Valores.ValorPis        := Leitor.rCampo(tcDe2, 'ValorPIS');
    NFSe.Servico.Valores.ValorCofins     := Leitor.rCampo(tcDe2, 'ValorCOFINS');
    NFSe.Servico.Valores.ValorInss       := Leitor.rCampo(tcDe2, 'ValorINSS');
    NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIR');
    NFSe.Servico.Valores.ValorCsll       := Leitor.rCampo(tcDe2, 'ValorCSLL');
    NFSe.Servico.Valores.AliquotaPIS     := Leitor.rCampo(tcDe2, 'AliquotaPIS');
    NFSe.Servico.Valores.AliquotaCOFINS := Leitor.rCampo(tcDe2, 'AliquotaCOFINS');
    NFSe.Servico.Valores.AliquotaINSS   := Leitor.rCampo(tcDe2, 'AliquotaINSS');
    NFSe.Servico.Valores.AliquotaIR     := Leitor.rCampo(tcDe2, 'AliquotaIR');
    NFSe.Servico.Valores.AliquotaCSLL   := Leitor.rCampo(tcDe2, 'AliquotaCSLL');


   	NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipalTomador');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadualTomador');

   	NFSe.Tomador.RazaoSocial   := Leitor.rCampo(tcStr, 'RazaoSocialTomador');
    NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'EmailTomador');

    NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'Discriminacao');
  end;

  SetxItemListaServico;

  if (Leitor.rExtrai(2, 'EnderecoTomador') <> '') then
  begin
    NFSe.Tomador.Endereco.Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
    NFSe.Tomador.Endereco.Numero          := Leitor.rCampo(tcStr, 'NumeroEndereco');
    NFSe.Tomador.Endereco.Complemento     := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.Tomador.Endereco.Bairro          := Leitor.rCampo(tcStr, 'Bairro');
    NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'UF');
    NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
    NFSe.Tomador.Endereco.CEP             := Leitor.rCampo(tcStr, 'CEP');
  end;

  if (Leitor.rExtrai(1, 'ChaveRPS') <> '') then
  begin
    NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoPrestador');
    NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'SerieRPS');
    NFSe.IdentificacaoRps.Tipo   := trRPS;//StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRPS');
    NFSe.InfID.ID                := OnlyNumber(NFSe.IdentificacaoRps.Numero);// + NFSe.IdentificacaoRps.Serie;
  end;


  if (Leitor.rExtrai(1, 'CPFCNPJTomador') <> '') then
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampoCNPJCPF;

  Result := True;
end;

function TNFSeR.LerRPS_Equiplano: Boolean;
var
  ok: Boolean;
  Item: Integer;
begin
  NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'nrRps');
  NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'nrEmissorRps');

  NFSe.DataEmissao      := Leitor.rCampo(tcDatHor, 'dtEmissaoRps');
  NFSe.DataEmissaoRps   := Leitor.rCampo(tcDat, 'DataEmissao');
  NFSe.NaturezaOperacao := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));

  NFSe.Servico.Valores.IssRetido        := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'isIssRetido'));
  NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'vlLiquidoRps');

  if (Leitor.rExtrai(2, 'tomador') <> '') then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj              := Leitor.rCampo(tcStr, 'nrDocumento');
    NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro:= Leitor.rCampo(tcStr, 'dsDocumentoEstrangeiro');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual    := Leitor.rCampo(tcStr, 'nrInscricaoEstadual');

    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'nmTomador');

    NFSe.Tomador.Endereco.Endereco        := Leitor.rCampo(tcStr, 'dsEndereco');
    NFSe.Tomador.Endereco.Numero          := Leitor.rCampo(tcStr, 'nrEndereco');
    NFSe.Tomador.Endereco.Complemento     := Leitor.rCampo(tcStr, 'dsComplemento');
    NFSe.Tomador.Endereco.Bairro          := Leitor.rCampo(tcStr, 'nmBairro');
    NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'nrCidadeIbge');
    NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'nmUf');
    NFSe.Tomador.Endereco.xPais           := Leitor.rCampo(tcStr, 'nmPais');
    NFSe.Tomador.Endereco.CEP             := Leitor.rCampo(tcStr, 'nrCep');

    NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'nrTelefone');
    NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'dsEmail');
  end;

  if (Leitor.rExtrai(2, 'listaServicos') <> '') then
  begin
    NFSe.Servico.ItemListaServico := Poem_Zeros( VarToStr( Leitor.rCampo(tcStr, 'nrServicoItem') ), 2) +
                                     Poem_Zeros( VarToStr( Leitor.rCampo(tcStr, 'nrServicoSubItem') ), 2);

    Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
    if Item < 100 then
      Item:=Item * 100 + 1;

    NFSe.Servico.ItemListaServico := FormatFloat('0000', Item);
    NFSe.Servico.ItemListaServico := Copy(NFSe.Servico.ItemListaServico, 1, 2) + '.' +
                                     Copy(NFSe.Servico.ItemListaServico, 3, 2);

    if TabServicosExt then
      NFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
    else
     NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

    NFSe.Servico.Valores.ValorServicos        := Leitor.rCampo(tcDe2, 'vlServico');
    NFSe.Servico.Valores.Aliquota             := Leitor.rCampo(tcDe2, 'vlAliquota');
    NFSe.Servico.Valores.ValorDeducoes        := Leitor.rCampo(tcDe2, 'vlDeducao');
    NFSe.Servico.Valores.JustificativaDeducao := Leitor.rCampo(tcStr, 'dsJustificativaDeducao');
    NFSe.Servico.Valores.BaseCalculo          := Leitor.rCampo(tcDe2, 'vlBaseCalculo');
    NFSe.Servico.Valores.ValorIss             := Leitor.rCampo(tcDe2, 'vlIssServico');
    NFSe.Servico.Discriminacao                := Leitor.rCampo(tcStr, 'dsDiscriminacaoServico');
  end;

  if (Leitor.rExtrai(2, 'retencoes') <> '') then
  begin
    NFSe.Servico.Valores.ValorCofins    := Leitor.rCampo(tcDe2, 'vlCofins');
    NFSe.Servico.Valores.ValorCsll      := Leitor.rCampo(tcDe2, 'vlCsll');
    NFSe.Servico.Valores.ValorInss      := Leitor.rCampo(tcDe2, 'vlInss');
    NFSe.Servico.Valores.ValorIr        := Leitor.rCampo(tcDe2, 'vlIrrf');
    NFSe.Servico.Valores.ValorPis       := Leitor.rCampo(tcDe2, 'vlPis');
    NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'vlIss');
    NFSe.Servico.Valores.AliquotaCofins := Leitor.rCampo(tcDe2, 'vlAliquotaCofins');
    NFSe.Servico.Valores.AliquotaCsll   := Leitor.rCampo(tcDe2, 'vlAliquotaCsll');
    NFSe.Servico.Valores.AliquotaInss   := Leitor.rCampo(tcDe2, 'vlAliquotaInss');
    NFSe.Servico.Valores.AliquotaIr     := Leitor.rCampo(tcDe2, 'vlAliquotaIrrf');
    NFSe.Servico.Valores.AliquotaPis    := Leitor.rCampo(tcDe2, 'vlAliquotaPis');
  end;

  Result := True;
end;

function TNFSeR.LerRps_Governa: Boolean;
begin
  Leitor.rExtrai(1, 'LoteRps');
  NFSe.dhRecebimento                := StrToDateTime(formatdatetime ('dd/mm/yyyy',now));
  NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'CodCadBic');
  NFSe.Prestador.ChaveAcesso        := Leitor.rCampo(tcStr, 'ChvAcs');
  NFSe.CodigoVerificacao            := Leitor.rCampo(tcStr, 'CodVer');
  NFSe.IdentificacaoRps.Numero      := Leitor.rCampo(tcStr, 'NumRps');
  Result := True;
end;

////////////////////////////////////////////////////////////////////////////////
//  Funções especificas para ler o XML de uma NFS-e                           //
////////////////////////////////////////////////////////////////////////////////

function TNFSeR.LerNFSe: Boolean;
var
  ok: Boolean;
  CM: String;
  DataHorBR: String;
begin
  if FProvedor = proNenhum then
  begin
    if (Leitor.rExtrai(1, 'OrgaoGerador') <> '') then
    begin
      CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      FProvedor := CodCidadeToProvedor(CM);
    end;

    if FProvedor = proNenhum then
    begin
      if (Leitor.rExtrai(1, 'PrestadorServico') <> '') then
      begin
        CM := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoMunicipio'));
        if CM = '' then
          CM := Leitor.rCampo(tcStr, 'Cidade');
        FProvedor := CodCidadeToProvedor(CM);
      end;
    end;

    if FProvedor = proNenhum then
    begin
      // tags do xml baixado do provedor
      if (Pos('<numero_nfse>', Leitor.Arquivo) > 0) and
         (Pos('<serie_nfse>', Leitor.Arquivo) > 0) then 
        FProvedor := proIPM;
    end;

    if FProvedor = proNenhum then
    begin
      if (Pos('<nfs', Leitor.Arquivo) > 0) then
        FProvedor := proEquiplano;
    end;

    if FProvedor = proNenhum then
    begin
      if (Leitor.rExtrai(1, 'Servico') <> '') then
      begin
        CM := Leitor.rCampo(tcStr, 'CodigoMunicipio');
        FProvedor := CodCidadeToProvedor(CM);
      end;
    end;

    if FProvedor = proNenhum then
      FProvedor := FProvedorConf;
  end;

  VersaoNFSe := ProvedorToVersaoNFSe(FProvedor);
  LayoutXML := ProvedorToLayoutXML(FProvedor);

  if (Leitor.rExtrai(1, 'Nfse') <> '') or (Pos('Nfse versao="2.01"', Leitor.Arquivo) > 0) then
  begin
    NFSe.InfID.ID := Leitor.rAtributo('Id=', 'InfNfse');
    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := Leitor.rAtributo('id=', 'InfNfse');

    // Quando baixamos diretamente do site do provedor GINFES dentro do grupo <Nfse> não
    // contem o grupo <InfNfse>
    if (Leitor.rExtrai(2, 'InfNfse') = '') and (Leitor.rExtrai(1, 'InfNfse') = '') then
      Leitor.Grupo := Leitor.Arquivo;

    Nivel := 0;

    if (Leitor.rExtrai(2, 'InfNfse') <> '') then
      Nivel := 2;
    if (Leitor.rExtrai(1, 'InfNfse') <> '')  or (Leitor.rExtrai(1, 'Nfse') <> '') then
      Nivel := 1;

    if Nivel > 0 then
    begin
      if FProvedor = proTecnos then
        NFSe.Link := Leitor.rCampo(tcStr, 'LinkNota');

      NFSe.Numero            := Leitor.rCampo(tcStr, 'Numero');
      NFSe.SeriePrestacao    := Leitor.rCampo(tcStr, 'Serie');
      NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');

      {Considerar a data de recebimento da NFS-e como dhrecebimento - para esse provedor nao tem a tag
        Diferente do que foi colocado para outros provedores, de atribuir a data now, ficaria errado se
        passase a transmissao de um dia para outro. E se for pensar como dhrecebimento pelo webservice e
        não o recebimento no programa que usar esse componente
      }
      if FProvedor = proVersaTecnologia then
        NFSe.dhRecebimento := Leitor.rCampo(tcDatHor, 'DataEmissao');

      case FProvedor of
        proFreire,
        proSpeedGov,
        proVitoria,
        proDBSeller,
        proFriburgo: NFSe.DataEmissao := Leitor.rCampo(tcDat, 'DataEmissao');

        proNFSeBrasil:
          begin
            DataHorBR := Leitor.rCampo(tcStr, 'DataEmissao');

            NFSe.DataEmissao := StringToDateTime(DataHorBr, 'DD/MM/YYYY hh:nn:ss');
          end;
      else
        NFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');
      end;

      // Tratar erro de conversão de tipo no Provedor Ábaco
      if Leitor.rCampo(tcStr, 'DataEmissaoRps') <> '0000-00-00' then
      begin
        if FProvedor in [proNFSeBrasil] then
        begin
          DataHorBR := Leitor.rCampo(tcStr, 'DataEmissaoRps');
          NFSe.DataEmissaoRps := StringToDateTime(DataHorBr, 'DD/MM/YYYY');
        end
        else
          NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissaoRps');
      end;

      NFSe.NaturezaOperacao         := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
      NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
      NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));

      if FProvedor = ProTecnos then
        NFSe.Competencia := DateTimeToStr(StrToFloatDef(Leitor.rCampo(tcDatHor, 'Competencia'), 0))
      else
        NFSe.Competencia := Leitor.rCampo(tcStr, 'Competencia');

      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'OutrasInformacoes');
      NFSe.ValorCredito      := Leitor.rCampo(tcDe2, 'ValorCredito');

      NFSe.InformacoesComplementares := Leitor.rCampo(tcStr, 'InformacoesComplementares');

      if FProvedor = proVitoria then
        NFSe.IncentivadorCultural := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'))
      else
        NFSe.IncentivadorCultural := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));

      if FProvedor = proISSNet then
        FNFSe.NfseSubstituida := ''
      else
      begin
        NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida');
        if NFSe.NfseSubstituida = '' then
          NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituta');
      end;
    end;
  end;

  NFSe.Cancelada := snNao;
  NFSe.Status := srNormal;

  if FProvedor = proGoverna then
    Leitor.rExtrai(1, 'RetornoConsultaRPS');

  case LayoutXML of
    loABRASFv1:    Result := LerNFSe_ABRASF_V1;
    loABRASFv2:    Result := LerNFSe_ABRASF_V2;
    loEL:          Result := LerNFSe_EL;
    loEGoverneISS: Result := False; // Falta implementar
    loEquiplano:   Result := LerNFSe_Equiplano;
    loGoverna:     Result := LerNFSe_Governa;
    loInfisc:      Result := LerNFSe_Infisc;
    loISSDSF:      Result := LerNFSe_ISSDSF;
    loCONAM:       Result := LerNFSe_CONAM;
    loAgili:       Result := LerNFSe_Agili;
    loSP:          Result := LerNFSe_SP;
    loSMARAPD:     Result := LerNFSe_Smarapd;
    loIPM:         Result := LerNFSe_IPM;
  else
    Result := False;
  end;

  Leitor.Grupo := Leitor.Arquivo;
  if FProvedor = proABase then
  begin
    NFSe.Status := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status') );
    if NFSe.Status = srCancelado then
      NFSe.Cancelada := snSim
    else
      NFSe.Cancelada := snNao;
  end;  

  if ((Leitor.rExtrai(1, 'NfseCancelamento') <> '') or (Leitor.rExtrai(1, 'CancelamentoNfse') <> '')) then
  begin
    NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHora');
    if NFSe.NfseCancelamento.DataHora = 0 then
      NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'DataHoraCancelamento');
    NFSe.NfseCancelamento.Pedido.CodigoCancelamento := Leitor.rCampo(tcStr, 'CodigoCancelamento');

    case FProvedor of
     proBetha: begin
                 if NFSe.NfseCancelamento.DataHora <> 0 then
                 begin
                   NFSe.Cancelada := snSim;
                   NFSE.Status := srCancelado;
                 end;
               end;
    else begin
           NFSe.Cancelada := snSim;
           NFSE.Status := srCancelado;
         end;
    end;
  end;

  if (Leitor.rExtrai(1, 'NfseSubstituicao') <> '') then
    NFSe.NfseSubstituidora := Leitor.rCampo(tcStr, 'NfseSubstituidora');
end;

function TNFSeR.LerNFSe_ABRASF_V1: Boolean;
var
  I: Integer;
  ok: Boolean;
begin
  if FProvedor <> proNFSeBrasil then
  begin
    if (Leitor.rExtrai(Nivel +1, 'IdentificacaoRps') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      if NFSe.InfID.ID = '' then
        NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;
  end
  else
  begin
    NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRps');
    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
  end;

  if (Leitor.rExtrai(Nivel +1, 'Servico') <> '') then
  begin
    SetxItemListaServico;

    NFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
    NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
    NFSe.Servico.Descricao                 := '';
    if FProvedor = proISSNet then
      NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio')
    else
      NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

//    NFSe.Servico.ResponsavelRetencao       := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
//    NFSe.Servico.CodigoPais          := Leitor.rCampo(tcInt, 'CodigoPais');
//    NFSe.Servico.ExigibilidadeISS    := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));
//    NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');
//    if NFSe.Servico.MunicipioIncidencia =0
//     then NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'CodigoMunicipio');

    if (Leitor.rExtrai(Nivel +2, 'Valores') <> '') then
    begin
      NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
      NFSe.Servico.Valores.ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
      NFSe.Servico.Valores.ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
      NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
      NFSe.Servico.Valores.ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
      NFSe.Servico.Valores.ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
      NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
      NFSe.Servico.Valores.IssRetido              := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
      NFSe.Servico.Valores.ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
      NFSe.Servico.Valores.OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
      NFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');
      NFSe.Servico.Valores.Aliquota               := Leitor.rCampo(tcDe3, 'Aliquota');

      if NFSe.Servico.Valores.Aliquota = 0 then
        NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'AliquotaServicos');

      NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
      NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
      NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
      NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');
    end;

    if NFSe.Servico.Valores.ValorLiquidoNfse = 0 then
      NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                               NFSe.Servico.Valores.DescontoIncondicionado -
                                               NFSe.Servico.Valores.DescontoCondicionado -
                                               // Retenções Federais
                                               NFSe.Servico.Valores.ValorPis -
                                               NFSe.Servico.Valores.ValorCofins -
                                               NFSe.Servico.Valores.ValorIr -
                                               NFSe.Servico.Valores.ValorInss -
                                               NFSe.Servico.Valores.ValorCsll -

                                               NFSe.Servico.Valores.OutrasRetencoes -
                                               NFSe.Servico.Valores.ValorIssRetido;

    if NFSe.Servico.Valores.BaseCalculo = 0 then
      NFSe.Servico.Valores.BaseCalculo := NFSe.Servico.Valores.ValorServicos -
                                          NFSe.Servico.Valores.ValorDeducoes -
                                          NFSe.Servico.Valores.DescontoIncondicionado;

//    if NFSe.Servico.Valores.ValorIss = 0 then
//      NFSe.Servico.Valores.ValorIss := (NFSe.Servico.Valores.BaseCalculo * NFSe.Servico.Valores.Aliquota)/100;

    // Provedor SimplISS permite varios itens servico
    if FProvedor = proSimplISS then
    begin
      i := 1;
      while (Leitor.rExtrai(Nivel +2, 'ItensServico', 'ItensServico', i) <> '') do
      begin
        with NFSe.Servico.ItemServico.Add do
        begin
          Descricao := Leitor.rCampo(tcStr, 'Descricao');
          Quantidade := Leitor.rCampo(tcInt, 'Quantidade');
          ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
        end;
        inc(i);
      end;
    end;

  end; // fim serviço

  if Leitor.rExtrai(Nivel +1, 'PrestadorServico') <> '' then
  begin
    NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

    NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.PrestadorServico.Endereco.Endereco = '' then
    begin
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<Endereco>'
       then NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
    end;

    NFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    if NFSe.PrestadorServico.Endereco.CodigoMunicipio = '' then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');

    NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    if NFSe.PrestadorServico.Endereco.UF = '' then
      NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

//    NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
    NFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio) < 7 then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
          FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

    NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

    if (Leitor.rExtrai(Nivel +2, 'IdentificacaoPrestador') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');

      if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
        if Leitor.rExtrai(Nivel +3, 'CpfCnpj') <> '' then
        begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
          if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
            NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
    end;

    if Leitor.rExtrai(Nivel +2, 'Contato') <> '' then
    begin
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end;

  end; // fim PrestadorServico

  if (Leitor.rExtrai(Nivel +1, 'Tomador') <> '') or (Leitor.rExtrai(Nivel +1, 'TomadorServico') <> '') then
  begin
    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := Leitor.rCampo(tcStr, 'InscricaoEstadual');

    NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.Tomador.Endereco.Endereco = '' then
    begin
      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>'
       then NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);
    end;

    NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    if NFSe.Tomador.Endereco.CodigoMunicipio = '' then
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');

    NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    if NFSe.Tomador.Endereco.UF = '' then
      NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

    NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7 then
      NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
           FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

    if NFSe.Tomador.Endereco.UF = '' then
      NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

     NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

    if Leitor.rExtrai(Nivel +2, 'IdentificacaoTomador') <> '' then
    begin
      NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      if Leitor.rExtrai(Nivel +3, 'CpfCnpj') <> '' then
      begin
        if Leitor.rCampo(tcStr, 'Cpf') <> '' then
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
        else if Leitor.rCampo(tcStr, 'Cnpj') <> '' then
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj')
        else
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'CpfCnpj');
      end;
    end;

    if Leitor.rExtrai(Nivel +2, 'Contato') <> '' then
    begin
      NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end;
  end;

  if Leitor.rExtrai(Nivel +1, 'IntermediarioServico') <> '' then
  begin
    NFSe.IntermediarioServico.RazaoSocial        := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.IntermediarioServico.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
    if Leitor.rExtrai(Nivel +2, 'CpfCnpj') <> '' then
    begin
      if Leitor.rCampo(tcStr, 'Cpf')<>'' then
        NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
      else
        NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
    end;
  end;

  if Leitor.rExtrai(Nivel +1, 'OrgaoGerador') <> '' then
  begin
    NFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    NFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
  end; // fim OrgaoGerador

  if Leitor.rExtrai(Nivel +1, 'ConstrucaoCivil') <> '' then
  begin
    NFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
    NFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
  end;

  if FProvedor in [proBetha] then
    if Leitor.rExtrai(Nivel +1, 'CondicaoPagamento') <> '' then
    begin
      NFSe.CondicaoPagamento.Condicao:= StrToCondicao(ok,Leitor.rCampo(tcStr,'Condicao'));
      NFSe.CondicaoPagamento.QtdParcela:= Leitor.rCampo(tcInt,'Condicao');
      for I := 0 to 9999 do
      begin
        if (Leitor.rExtrai(Nivel +2, 'Parcelas', 'Parcelas', i) <> '') then
        begin
          with NFSe.CondicaoPagamento.Parcelas.Add do
          begin
            Parcela        := Leitor.rCampo(tcInt, 'Parcela');
            DataVencimento := Leitor.rCampo(tcDatVcto, 'DataVencimento');
            Valor          := Leitor.rCampo(tcDe2, 'Valor');
          end;
        end
        else
          Break;
      end;
    end;

  Result := True;
end;

function TNFSeR.LerNFSe_ABRASF_V2: Boolean;
var
  Nivel: Integer;
  ok: Boolean;
begin
  if Leitor.rExtrai(3, 'ValoresNfse') <> '' then
  begin
    NFSe.ValoresNfse.BaseCalculo      := Leitor.rCampo(tcDe2, 'BaseCalculo');
    NFSe.ValoresNfse.Aliquota         := Leitor.rCampo(tcDe3, 'Aliquota');
    NFSe.ValoresNfse.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
    NFSe.ValoresNfse.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');

    if (FProvedor in [proCoplan, proWebISSv2, proTiplanv2]) then
    begin
      NFSe.Servico.Valores.BaseCalculo      := Leitor.rCampo(tcDe2, 'BaseCalculo');
      NFSe.Servico.Valores.Aliquota         := Leitor.rCampo(tcDe3, 'Aliquota');
      NFSe.Servico.Valores.ValorIss         := Leitor.rCampo(tcDe2, 'ValorIss');
      NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
    end;
  end; // fim ValoresNfse

  if (Leitor.rExtrai(3, 'PrestadorServico') <> '') or (Leitor.rExtrai(3, 'DadosPrestador') <> '') then
  begin
    NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

    NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');

    if NFSe.PrestadorServico.Endereco.Endereco = '' then
    begin
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');

      if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<Endereco>' then
        NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
    end;

    NFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    if NFSe.PrestadorServico.Endereco.CodigoMunicipio = '' then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');

    NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    if NFSe.PrestadorServico.Endereco.UF = '' then
      NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

    NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
    NFSe.PrestadorServico.Endereco.CEP        := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio) < 7 then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
          FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

    NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

    if (Leitor.rExtrai(4, 'IdentificacaoPrestador') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');

      if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
        if Leitor.rExtrai(5, 'CpfCnpj') <> '' then
        begin
          NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
           if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
             NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
        end;
    end;

    if Leitor.rExtrai(4, 'Contato') <> '' then
    begin
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end;

  end; // fim PrestadorServico

  if Leitor.rExtrai(3, 'EnderecoPrestadorServico') <> '' then
  begin
    NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.PrestadorServico.Endereco.Endereco = '' then
    begin
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');

      if Copy(NFSe.PrestadorServico.Endereco.Endereco, 1, 10) = '<Endereco>' then
        NFSe.PrestadorServico.Endereco.Endereco := Copy(NFSe.PrestadorServico.Endereco.Endereco, 11, 125);
    end;

    NFSe.PrestadorServico.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.PrestadorServico.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    if NFSe.PrestadorServico.Endereco.CodigoMunicipio = '' then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');

    NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    if NFSe.PrestadorServico.Endereco.UF = '' then
      NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

    NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
    NFSe.PrestadorServico.Endereco.CEP        := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio) < 7 then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
           FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

    NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

  end; // fim EnderecoPrestadorServico

  if Leitor.rExtrai(3, 'ContatoPrestadorServico') <> '' then
  begin
    NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
    NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
  end; // fim ContatoPrestadorServico

  if Leitor.rExtrai(3, 'OrgaoGerador') <> '' then
  begin
    NFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    NFSe.OrgaoGerador.Uf              := Leitor.rCampo(tcStr, 'Uf');
  end; // fim OrgaoGerador

  if Leitor.rExtrai(3, 'ConstrucaoCivil') <> '' then
  begin
    NFSe.ConstrucaoCivil.CodigoObra := Leitor.rCampo(tcStr, 'CodigoObra');
    NFSe.ConstrucaoCivil.Art        := Leitor.rCampo(tcStr, 'Art');
  end;

  if Leitor.rExtrai(3, 'Intermediario') <> '' then
  begin
    NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
  end;

  if (Leitor.rExtrai(3, 'InfDeclaracaoPrestacaoServico') <> '') or
     (Leitor.rExtrai(3, 'DeclaracaoPrestacaoServico') <> '') then
    Nivel := 4
  else
    Nivel := 3;

  if FProvedor = proSystemPro then
  begin
    NFSe.InfID.ID := Leitor.rAtributo('Id=');
    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := Leitor.rAtributo('id=');
  end;

  if FProvedor = ProTecnos then
    NFSe.Competencia := DateTimeToStr(StrToFloatDef(Leitor.rCampo(tcDatHor, 'Competencia'), 0))
  else
    NFSe.Competencia := Leitor.rCampo(tcStr, 'Competencia');

  NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
  NFSe.OptanteSimplesNacional   := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
  NFSe.IncentivadorCultural     := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));

  if (Leitor.rExtrai(Nivel, 'Rps') <> '') then
  begin
    NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
    NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

    if (Leitor.rExtrai(Nivel+1, 'IdentificacaoRps') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      if NFSe.InfID.ID = '' then
        NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;

    if (Leitor.rExtrai(Nivel+1, 'RpsSubstituido') <> '') then
    begin
      NFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    end;
  end
  else
  begin
    NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
    NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

    if (Leitor.rExtrai(Nivel, 'IdentificacaoRps') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
      if NFSe.InfID.ID = '' then
        NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;

    if (Leitor.rExtrai(Nivel, 'RpsSubstituido') <> '') then
    begin
      NFSe.RpsSubstituido.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.RpsSubstituido.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.RpsSubstituido.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    end;
  end;

  if (Leitor.rExtrai(Nivel, 'Servico') <> '') then
  begin
    NFSe.Servico.Valores.IssRetido   := StrToSituacaoTributaria(ok, Leitor.rCampo(tcStr, 'IssRetido'));
    NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));

    SetxItemListaServico;

    NFSe.Servico.CodigoCnae                := Leitor.rCampo(tcStr, 'CodigoCnae');
    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoTributacaoMunicipio');
    NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'Discriminacao');
    NFSe.Servico.Descricao                 := Leitor.rCampo(tcStr, 'Descricao');
    NFSe.Servico.CodigoMunicipio           := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    NFSe.Servico.CodigoPais                := Leitor.rCampo(tcInt, 'CodigoPais');

    if (FProvedor = proABAse) then
      NFSe.Servico.ExigibilidadeISS := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'Exigibilidade'))
    else
      NFSe.Servico.ExigibilidadeISS := StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeISS'));

    NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');

    if NFSe.Servico.MunicipioIncidencia = 0 then
      NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'CodigoMunicipio');

    if (Leitor.rExtrai(Nivel+1, 'Valores') <> '') then
    begin
      NFSe.Servico.Valores.ValorServicos   := Leitor.rCampo(tcDe2, 'ValorServicos');
      NFSe.Servico.Valores.ValorDeducoes   := Leitor.rCampo(tcDe2, 'ValorDeducoes');
      NFSe.Servico.Valores.ValorPis        := Leitor.rCampo(tcDe2, 'ValorPis');
      NFSe.Servico.Valores.ValorCofins     := Leitor.rCampo(tcDe2, 'ValorCofins');
      NFSe.Servico.Valores.ValorInss       := Leitor.rCampo(tcDe2, 'ValorInss');
      NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIr');
      NFSe.Servico.Valores.ValorCsll       := Leitor.rCampo(tcDe2, 'ValorCsll');
      NFSe.Servico.Valores.OutrasRetencoes := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
//        NFSe.Servico.Valores.BaseCalculo            := Leitor.rCampo(tcDe2, 'BaseCalculo');

      if NFSe.Servico.Valores.ValorIss = 0 then
        NFSe.Servico.Valores.ValorIss := Leitor.rCampo(tcDe2, 'ValorIss');

      if NFSe.Servico.Valores.Aliquota = 0 then
        NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'Aliquota');

      if NFSe.Servico.Valores.Aliquota = 0 then
        NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe3, 'AliquotaServicos');

      if (FProvedor in [proActconv202]) then
        NFSe.Servico.Valores.Aliquota := (NFSe.Servico.Valores.Aliquota * 100);

      if (FProvedor in [proActconv202, proISSe, proVersaTecnologia, proNEAInformatica,
                        proFiorilli, proPronimv2, proVitoria, proSmarAPDABRASF,
                        proGovDigital]) then
      begin
        if NFSe.Servico.Valores.IssRetido = stRetencao then
          NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIss')
        else
          NFSe.Servico.Valores.ValorIssRetido := 0;
      end
      else
        NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'ValorIssRetido');

//        NFSe.Servico.Valores.ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
      NFSe.Servico.Valores.DescontoCondicionado   := Leitor.rCampo(tcDe2, 'DescontoCondicionado');
      NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'DescontoIncondicionado');

      if NFSe.Servico.Valores.ValorLiquidoNfse = 0 then
        NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                                 NFSe.Servico.Valores.DescontoIncondicionado -
                                                 NFSe.Servico.Valores.DescontoCondicionado -
                                                 // Retenções Federais
                                                 NFSe.Servico.Valores.ValorPis -
                                                 NFSe.Servico.Valores.ValorCofins -
                                                 NFSe.Servico.Valores.ValorIr -
                                                 NFSe.Servico.Valores.ValorInss -
                                                 NFSe.Servico.Valores.ValorCsll -

                                                 NFSe.Servico.Valores.OutrasRetencoes -
                                                 NFSe.Servico.Valores.ValorIssRetido;

      if NFSe.Servico.Valores.BaseCalculo = 0 then
        NFSe.Servico.Valores.BaseCalculo := NFSe.Servico.Valores.ValorServicos -
                                            NFSe.Servico.Valores.ValorDeducoes -
                                            NFSe.Servico.Valores.DescontoIncondicionado;

//        if NFSe.Servico.Valores.ValorIss = 0 then
//          NFSe.Servico.Valores.ValorIss := (NFSe.Servico.Valores.BaseCalculo * NFSe.Servico.Valores.Aliquota)/100;

    end;
  end; // fim serviço

  if (Leitor.rExtrai(Nivel, 'Prestador') <> '') then
  begin
    NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

    if (VersaoNFSe = ve100) or
       (FProvedor in [proFiorilli, proGoiania, ProTecnos, proVirtual,
                      proDigifred, proNEAInformatica]) then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
      if (FProvedor = proTecnos) then
        NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
      if Leitor.rExtrai(Nivel+1, 'CpfCnpj') <> '' then
      begin
         NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
         if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
           NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;
      NFSe.Prestador.Cnpj := NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
    end
    else
    begin
      NFSe.Prestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := NFSe.Prestador.Cnpj;
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSe.Prestador.InscricaoMunicipal;
    end;
  end; // fim Prestador

  if (Leitor.rExtrai(Nivel, 'TomadorServico') <> '') or
     (Leitor.rExtrai(Nivel, 'Tomador') <> '') then
  begin
    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

    NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'EnderecoDescricao');
    if NFSe.Tomador.Endereco.Endereco = '' then
    begin
      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Endereco');
      if Copy(NFSe.Tomador.Endereco.Endereco, 1, 10) = '<Endereco>' then
        NFSe.Tomador.Endereco.Endereco := Copy(NFSe.Tomador.Endereco.Endereco, 11, 125);
    end;

    NFSe.Tomador.Endereco.Numero      := Leitor.rCampo(tcStr, 'Numero');
    NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.Tomador.Endereco.Bairro      := Leitor.rCampo(tcStr, 'Bairro');

    NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
    if NFSe.Tomador.Endereco.CodigoMunicipio = '' then
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');

    NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    if NFSe.Tomador.Endereco.UF = '' then
      NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Estado');

    NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7 then
      NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
         FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

    if NFSe.Tomador.Endereco.UF = '' then
      NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

    NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

    if (Leitor.rExtrai(Nivel+1, 'IdentificacaoTomador') <> '') then
    begin
      NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

      if Leitor.rExtrai(Nivel+2, 'CpfCnpj') <> '' then
      begin
        if Leitor.rCampo(tcStr, 'Cpf')<>'' then
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
        else
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;
    end;

    if (Leitor.rExtrai(Nivel+1, 'Contato') <> '') then
    begin
      NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end;
  end; // fim Tomador

  Result := True;
end;

function TNFSeR.LerNFSe_Agili: Boolean;
var
  ok: Boolean;
  i: Integer;
  itemServico: TItemServicoCollectionItem;
  codCNAE: Variant;
  codLCServ: string;
  ValorServicosTotal: Currency;

  function _StrToSimNao(out ok: boolean; const s: String): TnfseSimNao;
  begin
    result := StrToEnumerado(ok, s,
                             ['1','0'],
                             [snSim, snNao]);
  end;

  function _StrToResponsavelRetencao(out ok: boolean; const s: String): TnfseResponsavelRetencao;
  begin
    result := StrToEnumerado(ok, s,
                             ['-1', '-2', '-3'],
                             [ptTomador, rtPrestador, rtPrestador]);
  end;

  function _StrToRegimeEspecialTributacao(out ok: boolean; const s: String): TnfseRegimeEspecialTributacao;
  begin
    // -7 Microempresario individual MEI optante pelo SIMEI
    result := StrToEnumerado(ok, s,
                            ['-1','-2','-4','-5','-6'],
                            [retNenhum, retEstimativa, retCooperativa,
                             retMicroempresarioIndividual, retMicroempresarioEmpresaPP
                            ]);
  end;

  function _StrToExigibilidadeISS(out ok: boolean; const s: String): TnfseExigibilidadeISS;
  begin
    // -8 Fixo
    result := StrToEnumerado(ok, s,
                            ['-1','-2','-3','-4','-5','-6','-7'],
                             [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
                              exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
  end;

  function _StrToTipoRPS(out ok: boolean; const s: String): TnfseTipoRPS;
  begin
    result := StrToEnumerado(ok, s,
                             ['-2','-4','-5'],
                             [trRPS, trNFConjugada, trCupom]);
  end;

begin
  codLCServ := '';

  ValorServicosTotal := 0;
  if (Leitor.rExtrai(1, 'InfDeclaracaoPrestacaoServico') <> '') or
     (Leitor.rExtrai(1, 'DeclaracaoPrestacaoServico') <> '') then
  begin
    NFSe.Servico.Valores.ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
  end;

  if (Leitor.rExtrai(1, 'ListaServico') <> '') then
  begin
    i := 1;
    while (Leitor.rExtrai(2, 'DadosServico', '', i) <> '') do
    begin
      itemServico := NFSe.Servico.ItemServico.Add;
      itemServico.Descricao := Leitor.rCampo(tcStr, 'Discriminacao');
      itemServico.Discriminacao := itemServico.Descricao;
      itemServico.ValorServicos := Leitor.rCampo(tcDe2, 'ValorServico');
      itemServico.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'ValorDesconto');
      itemServico.Quantidade := Leitor.rCampo(tcDe6, 'Quantidade');

      if VersaoNFSe = ve100 then
        codCNAE := Leitor.rCampo(tcStr, 'CodigoCnae');

      itemServico.CodLCServ := Leitor.rCampo(tcStr, 'ItemLei116');

      if codLCServ = '' then
        codLCServ := itemServico.CodLCServ;

      ValorServicosTotal := ValorServicosTotal + itemServico.ValorServicos;
      inc(i);
    end;

    for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
    begin
      itemServico := NFSe.Servico.ItemServico.Items[I];
      if ValorServicosTotal = NFSe.Servico.Valores.ValorServicos then
      begin
        itemServico.ValorTotal := itemServico.ValorServicos;
      if itemServico.Quantidade = 0 then
        itemServico.ValorUnitario := 0
      else
        itemServico.ValorUnitario := itemServico.ValorServicos / itemServico.Quantidade;
      end
      else
      begin
        itemServico.ValorUnitario := itemServico.ValorServicos;
        itemServico.ValorTotal := itemServico.ValorUnitario * itemServico.Quantidade;
      end;
    end;

  end; // fim lista serviço

  if Leitor.rExtrai(1, 'Nfse') <> '' then
  begin
    NFSe.Numero := Leitor.rCampo(tcStr, 'Numero');
    NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoAutenticidade');
    NFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');

    if VersaoNFSe = ve200 then
    begin
      NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida');
      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'Observacao');
    end;
  end;

  if Leitor.rExtrai(1, 'SituacaoNfse') <> '' then
  begin
    NFSe.Situacao := Leitor.rCampo(tcStr, 'Codigo');
    case StrToInt(NFSe.Situacao) of
      -2: begin
        NFSe.Cancelada := snSim;
        // DataCancelamento
        // CodigoCancelamento
        // MotivoCancelamento
        NFSe.MotivoCancelamento := Leitor.rCampo(tcStr, 'MotivoCancelamento');
        // JustificativaCancelamento
      end;
      -8: NFSe.Cancelada := snNao;
    end;
  end;

  if Leitor.rExtrai(1, 'IdentificacaoOrgaoGerador') <> '' then
  begin
    if VersaoNFSe = ve100 then
      NFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipioIBGE')
    else
      NFSe.OrgaoGerador.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

    NFSe.OrgaoGerador.Uf := Leitor.rCampo(tcStr, 'Uf');
  end;

  if Leitor.rExtrai(1, 'DadosPrestador') <> '' then
  begin
    NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'RazaoSocial');
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'NomeFantasia');

    NFSe.PrestadorServico.Endereco.TipoLogradouro := Leitor.rCampo(tcStr, 'TipoLogradouro');
    NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'Logradouro');
    NFSe.PrestadorServico.Endereco.Numero := Leitor.rCampo(tcStr, 'Numero');
    NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.PrestadorServico.Endereco.Bairro := Leitor.rCampo(tcStr, 'Bairro');

    if VersaoNFSe = ve100 then
    begin
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipioIBGE');
      NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPaisBacen');
    end
    else
    begin
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
      NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'CodigoPais');
    end;

    NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    NFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.PrestadorServico.Endereco.CodigoMunicipio) < 7 then
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 1, 2) +
          FormatFloat('00000', StrToIntDef(Copy(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 3, 5), 0));

    NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

    if Leitor.rExtrai(2, 'Contato') <> '' then
    begin
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end;

  end;

  if (Leitor.rExtrai(1, 'IdentificacaoPrestador') <> '') then
  begin
    // NFSe.PrestadorServico.IdentificacaoPrestador.ChaveAcesso := Leitor.rCampo(tcStr, 'ChaveDigital');
    // Remover chave digital do XML da NFS-e 
    NFSe.PrestadorServico.IdentificacaoPrestador.ChaveAcesso := StringOfChar('*', 32);
    NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

    NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
    if (NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '') and (Leitor.rExtrai(5, 'CpfCnpj') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cpf');
      if NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj = '' then
        NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'Cnpj');
    end;

  end;

  NFSe.Prestador.Cnpj := NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
  NFSe.Prestador.InscricaoMunicipal := NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
  NFSe.Prestador.Senha := NFSe.PrestadorServico.IdentificacaoPrestador.Senha;
  NFSe.Prestador.FraseSecreta := NFSe.PrestadorServico.IdentificacaoPrestador.FraseSecreta;
  NFSe.Prestador.cUF := NFSe.PrestadorServico.IdentificacaoPrestador.cUF;
  NFSe.Prestador.InscricaoEstadual := NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoEstadual;
  NFSe.Prestador.ChaveAcesso := NFSe.PrestadorServico.IdentificacaoPrestador.ChaveAcesso;

  // DadosTomador
  // DadosIntermediario
  // DadosMaterialUsado

  if VersaoNFSe = ve100 then
  begin
    if Leitor.rExtrai(1, 'RegimeEspecialTributacao') <> '' then
      NFSe.RegimeEspecialTributacao := _StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'Codigo'));

    //  NFSe.IncentivadorCultural := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));

    if Leitor.rExtrai(1, 'ResponsavelISSQN') <> '' then
      NFSe.Servico.ResponsavelRetencao := _StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'Codigo'));

    if Leitor.rExtrai(1, 'ExigililidadeISSQN') <> '' then
      NFSe.Servico.ExigibilidadeISS := _StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'Codigo'));

    if Leitor.rExtrai(1, 'MunicipioIncidencia') <> '' then
    begin
      NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'CodigoMunicipioIBGE');
      NFSe.Servico.CodigoMunicipio := IntToStr(NFSe.Servico.MunicipioIncidencia);
    end;
  end;

  if (Leitor.rExtrai(1, 'InfDeclaracaoPrestacaoServico') <> '') or
     (Leitor.rExtrai(1, 'DeclaracaoPrestacaoServico') <> '') then
  begin
    if VersaoNFSe = ve100 then
      NFSe.NfseSubstituida := Leitor.rCampo(tcStr, 'NfseSubstituida')
    else
    begin
      NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
      //  NFSe.IncentivadorCultural := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivoFiscal'));
      NFSe.Servico.ResponsavelRetencao := StrToResponsavelRetencao(ok, Leitor.rCampo(tcStr, 'ResponsavelRetencao'));
      NFSe.Servico.ExigibilidadeISS := _StrToExigibilidadeISS(ok, Leitor.rCampo(tcStr, 'ExigibilidadeIss'));
      NFSe.Servico.MunicipioIncidencia := Leitor.rCampo(tcInt, 'MunicipioIncidencia');
      NFSe.Servico.CodigoMunicipio := IntToStr(NFSe.Servico.MunicipioIncidencia);
    end;

    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoAtividadeEconomica');
    NFSe.Servico.CodigoCnae := codCNAE;
    NFSe.Servico.ItemListaServico := codLCServ;

    if TabServicosExt then
      NFSe.Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(NFSe.Servico.ItemListaServico))
    else
      NFSe.Servico.xItemListaServico := CodigoToDesc(OnlyNumber(NFSe.Servico.ItemListaServico));

    if VersaoNFSe = ve100 then
    begin
      NFSe.Servico.NumeroProcesso := Leitor.rCampo(tcStr, 'BeneficioProcesso');
      NFSe.ValoresNfse.BaseCalculo := Leitor.rCampo(tcDe2, 'ValorBaseCalculoISSQN');
      NFSe.ValoresNfse.Aliquota := Leitor.rCampo(tcDe3, 'AliquotaISSQN');
      // ValorISSQNCalculado
      // ValorISSQNRecolher
      NFSe.ValoresNfse.ValorIss := Leitor.rCampo(tcDe2, 'ValorISSQNRecolher');
    end
    else
    begin
      NFSe.Servico.NumeroProcesso := Leitor.rCampo(tcStr, 'NumeroProcesso');
      NFSe.ValoresNfse.BaseCalculo := Leitor.rCampo(tcDe2, 'ValorBaseCalculoIss');
      NFSe.ValoresNfse.Aliquota := Leitor.rCampo(tcDe3, 'Aliquota');
      NFSe.ValoresNfse.ValorIss := Leitor.rCampo(tcDe2, 'ValorIss');
    end;

    NFSe.ValoresNfse.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquido');

    for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
      NFSe.Servico.ItemServico[I].Aliquota := NFSe.ValoresNfse.Aliquota;

    NFSe.Servico.Valores.ValorServicos := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'ValorDescontos');
    NFSe.Servico.Valores.DescontoCondicionado := 0;
    NFSe.Servico.Valores.ValorDeducoes   := 0; 
    NFSe.Servico.Valores.ValorPis        := Leitor.rCampo(tcDe2, 'ValorPis');
    NFSe.Servico.Valores.ValorCofins     := Leitor.rCampo(tcDe2, 'ValorCofins');
    NFSe.Servico.Valores.ValorInss       := Leitor.rCampo(tcDe2, 'ValorInss');
    NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIrrf');
    NFSe.Servico.Valores.ValorCsll       := Leitor.rCampo(tcDe2, 'ValorCsll');
    NFSe.Servico.Valores.OutrasRetencoes := Leitor.rCampo(tcDe2, 'ValorOutrasRetencoes');

    if VersaoNFSe = ve100 then
    begin
      NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIrrf');
      NFSe.Servico.Valores.BaseCalculo     := Leitor.rCampo(tcDe2, 'ValorBaseCalculoISSQN');
      NFSe.Servico.Valores.Aliquota        := Leitor.rCampo(tcDe3, 'AliquotaISSQN');
      NFSe.Servico.Valores.ValorIss        := Leitor.rCampo(tcDe2, 'ValorISSQNCalculado');
    end
    else
    begin
      NFSe.Servico.Valores.ValorIr         := Leitor.rCampo(tcDe2, 'ValorIr');
      NFSe.Servico.Valores.BaseCalculo     := Leitor.rCampo(tcDe2, 'ValorBaseCalculoIss');
      NFSe.Servico.Valores.Aliquota        := Leitor.rCampo(tcDe3, 'Aliquota');
      NFSe.Servico.Valores.ValorIss        := Leitor.rCampo(tcDe2, 'ValorIss');
    end;

    NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorLiquido');
    NFSe.Servico.Valores.ValorIssRetido := 0;

    if ((VersaoNFSe = ve100) and (_StrToSimNao(ok, Leitor.rCampo(tcStr, 'ISSQNRetido')) = snSim)) or
       ((VersaoNfse = ve200) and (_StrToSimNao(ok, Leitor.rCampo(tcStr, 'IssRetido')) = snSim)) then
      NFSe.Servico.Valores.ValorIssRetido := NFSe.Servico.Valores.ValorIss;

    if NFSe.Servico.Valores.ValorLiquidoNfse = 0 then
      NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                               NFSe.Servico.Valores.DescontoIncondicionado -
                                               NFSe.Servico.Valores.DescontoCondicionado -
                                               // Retenções Federais
                                               NFSe.Servico.Valores.ValorPis -
                                               NFSe.Servico.Valores.ValorCofins -
                                               NFSe.Servico.Valores.ValorIr -
                                               NFSe.Servico.Valores.ValorInss -
                                               NFSe.Servico.Valores.ValorCsll -

                                               NFSe.Servico.Valores.OutrasRetencoes -
                                               NFSe.Servico.Valores.ValorIssRetido;

    if NFSe.Servico.Valores.BaseCalculo = 0 then
      NFSe.Servico.Valores.BaseCalculo := NFSe.Servico.Valores.ValorServicos -
                                          NFSe.Servico.Valores.ValorDeducoes -
                                          NFSe.Servico.Valores.DescontoIncondicionado;

//    NFSe.InfID.ID := Leitor.rAtributo('Id=');
//    if NFSe.InfID.ID = '' then
//      NFSe.InfID.ID := Leitor.rAtributo('id=');

    NFSe.Competencia := FormatDateTime('mm/yyyy', NFSe.DataEmissao);
    NFSe.OptanteSimplesNacional := _StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));

    if VersaoNFSe = ve100 then
      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'Observacao');
  end;
  
  // Fim infDeclaracaoServico
  
  if (Leitor.rExtrai(2, 'Rps') <> '') then
  begin
    NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissao');
    //NFSe.Status         := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

    if (Leitor.rExtrai(3, 'IdentificacaoRps') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');

      if VersaoNFSe = ve100 then
        NFSe.IdentificacaoRps.Tipo := _StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'))
      else
        NFSe.IdentificacaoRps.Tipo := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));

      if NFSe.InfID.ID = '' then
        NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;
  end;

  if (Leitor.rExtrai(2, 'DadosTomador') <> '') then
  begin
    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

    NFSe.Tomador.Endereco.TipoLogradouro := Leitor.rCampo(tcStr, 'TipoLogradouro');
    NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'Logradouro');
    NFSe.Tomador.Endereco.Numero := Leitor.rCampo(tcStr, 'Numero');
    NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'Complemento');
    NFSe.Tomador.Endereco.Bairro := Leitor.rCampo(tcStr, 'Bairro');

    if VersaoNFSe = ve100 then
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipioIBGE')
    else
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');

    NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'Uf');
    NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'Cep');

    if length(NFSe.Tomador.Endereco.CodigoMunicipio) < 7 then
      NFSe.Tomador.Endereco.CodigoMunicipio := Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 1, 2) +
         FormatFloat('00000', StrToIntDef(Copy(NFSe.Tomador.Endereco.CodigoMunicipio, 3, 5), 0));

    if NFSe.Tomador.Endereco.UF = '' then
      NFSe.Tomador.Endereco.UF := NFSe.PrestadorServico.Endereco.UF;

    NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));

    if (Leitor.rExtrai(2, 'IdentificacaoTomador') <> '') then
    begin
      NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');

      if Leitor.rExtrai(3, 'CpfCnpj') <> '' then
      begin
        if Leitor.rCampo(tcStr, 'Cpf')<>'' then
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cpf')
        else
          NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'Cnpj');
      end;
    end;

    if (Leitor.rExtrai(2, 'Contato') <> '') then
    begin
      NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
      NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
    end;
  end;

  Result := True;
end;

function TNFSeR.LerNFSe_ISSDSF: Boolean;
var
  ok: Boolean;
  Item: Integer;
  sOperacao, sTributacao: String;
begin
  Leitor.Grupo := Leitor.Arquivo;

  if (Pos('<Notas>', Leitor.Arquivo) > 0) or
     (Pos('<Nota>', Leitor.Arquivo) > 0) or
     (Pos('<ConsultaNFSe>', Leitor.Arquivo) > 0) then
  begin
    VersaoNFSe := ve100; // para este provedor usar padrão "1".

    FNFSe.Numero := Leitor.rCampo(tcStr, 'NumeroNota');
    if (FNFSe.Numero = '') then
      FNFSe.Numero := Leitor.rCampo(tcStr, 'NumeroNFe');

    FNFSe.NumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
    FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
    if FNFSe.CodigoVerificacao = '' then
      FNFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificao');

    FNFSe.DataEmissaoRps := Leitor.rCampo(tcDatHor, 'DataEmissaoRPS');
    FNFSe.Competencia    := Copy(Leitor.rCampo(tcDat, 'DataEmissaoRPS'),7,4) + Copy(Leitor.rCampo(tcDat, 'DataEmissaoRPS'),4,2);
    FNFSe.DataEmissao    := Leitor.rCampo(tcDatHor, 'DataProcessamento');
    if (FNFSe.DataEmissao = 0) then
      FNFSe.DataEmissao  := FNFSe.DataEmissaoRps;

    FNFSe.Status := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'SituacaoRPS'),['N','C'],[srNormal, srCancelado]);

    NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRPS');
    NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'SerieRPS');
    NFSe.IdentificacaoRps.Tipo   := trRPS; //StrToTipoRPS(ok, leitorAux.rCampo(tcStr, 'Tipo'));

    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero);// + NFSe.IdentificacaoRps.Serie;
      
    NFSe.SeriePrestacao := Leitor.rCampo(tcStr, 'SeriePrestacao');

    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipalTomador');
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj            := Leitor.rCampo(tcStr, 'CPFCNPJTomador');

    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocialTomador');

    NFSe.Tomador.Endereco.TipoLogradouro := Leitor.rCampo(tcStr, 'TipoLogradouroTomador');
    NFSe.Tomador.Endereco.Endereco       := Leitor.rCampo(tcStr, 'LogradouroTomador');
    NFSe.Tomador.Endereco.Numero         := Leitor.rCampo(tcStr, 'NumeroEnderecoTomador');
    NFSe.Tomador.Endereco.Complemento    := Leitor.rCampo(tcStr, 'ComplementoEnderecoTomador');
    NFSe.Tomador.Endereco.TipoBairro     := Leitor.rCampo(tcStr, 'TipoBairroTomador');
    NFSe.Tomador.Endereco.Bairro         := Leitor.rCampo(tcStr, 'BairroTomador');

    if (Leitor.rCampo(tcStr, 'CidadeTomador') <> '') then
    begin
      NFSe.Tomador.Endereco.CodigoMunicipio := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'CidadeTomador')) ;
      NFSe.Tomador.Endereco.xMunicipio      := CodCidadeToCidade( StrToInt(NFSe.Tomador.Endereco.CodigoMunicipio) ) ;
      NFSe.Tomador.Endereco.UF              := CodigoParaUF( StrToInt(Copy(NFSe.Tomador.Endereco.CodigoMunicipio,1,2) ));
    end;

    NFSe.Tomador.Endereco.CEP  := Leitor.rCampo(tcStr, 'CEPTomador');
    NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'EmailTomador');

    NFSe.Servico.CodigoCnae        := Leitor.rCampo(tcStr, 'CodigoAtividade');
    NFSe.Servico.Valores.Aliquota  := Leitor.rCampo(tcDe3, 'AliquotaAtividade');
    NFSe.Servico.Valores.IssRetido := StrToEnumerado( ok, Leitor.rCampo(tcStr, 'TipoRecolhimento'),
                                                     ['A','R'], [ stNormal, stRetencao{, stSubstituicao}]);

    if (Leitor.rCampo(tcStr, 'MunicipioPrestacao') <> '') then
      NFSe.Servico.CodigoMunicipio := CodSiafiToCodCidade( Leitor.rCampo(tcStr, 'MunicipioPrestacao'));

    sOperacao   := AnsiUpperCase(Leitor.rCampo(tcStr, 'Operacao'));
    sTributacao := AnsiUpperCase(Leitor.rCampo(tcStr, 'Tributacao'));
    NFSe.TipoRecolhimento := AnsiUpperCase(Leitor.rCampo(tcStr, 'TipoRecolhimento'));

    if (sOperacao <> '') then
    begin
      if sOperacao[1] in ['A', 'B'] then
      begin
        if NFSe.Servico.CodigoMunicipio = NFSe.PrestadorServico.Endereco.CodigoMunicipio then
          NFSe.NaturezaOperacao := no1
        else
          NFSe.NaturezaOperacao := no2;
      end
      else if (sOperacao = 'C') and (sTributacao = 'C') then
           begin
             NFSe.NaturezaOperacao := no3;
           end
           else if (sOperacao = 'C') and (sTributacao = 'F') then
                begin
                  NFSe.NaturezaOperacao := no4;
                end
                else if (sOperacao = 'A') and (sTributacao = 'N') then
                     begin
                       NFSe.NaturezaOperacao := no7;
                     end;
    end;

    NFSe.Servico.Operacao := StrToOperacao(Ok, sOperacao);
    NFSe.Servico.Tributacao := StrToTributacao(Ok, sTributacao);

    NFSe.NaturezaOperacao := StrToEnumerado( ok,sTributacao, ['T','K'], [ NFSe.NaturezaOperacao, no5 ]);

    NFSe.OptanteSimplesNacional := StrToEnumerado( ok,sTributacao, ['T','H'], [ snNao, snSim ]);

    NFSe.DeducaoMateriais := StrToEnumerado( ok,sOperacao, ['A','B'], [ snNao, snSim ]);

    NFse.RegimeEspecialTributacao := StrToEnumerado( ok,sTributacao, ['T','M'], [ retNenhum, retMicroempresarioIndividual ]);

    NFSe.Servico.Valores.ValorPis       := Leitor.rCampo(tcDe2, 'ValorPIS');
    NFSe.Servico.Valores.ValorCofins    := Leitor.rCampo(tcDe2, 'ValorCOFINS');
    NFSe.Servico.Valores.ValorInss      := Leitor.rCampo(tcDe2, 'ValorINSS');
    NFSe.Servico.Valores.ValorIr        := Leitor.rCampo(tcDe2, 'ValorIR');
    NFSe.Servico.Valores.ValorCsll      := Leitor.rCampo(tcDe2, 'ValorCSLL');
    NFSe.Servico.Valores.AliquotaPIS    := Leitor.rCampo(tcDe2, 'AliquotaPIS');
    NFSe.Servico.Valores.AliquotaCOFINS := Leitor.rCampo(tcDe2, 'AliquotaCOFINS');
    NFSe.Servico.Valores.AliquotaINSS   := Leitor.rCampo(tcDe2, 'AliquotaINSS');
    NFSe.Servico.Valores.AliquotaIR     := Leitor.rCampo(tcDe2, 'AliquotaIR');
    NFSe.Servico.Valores.AliquotaCSLL   := Leitor.rCampo(tcDe2, 'AliquotaCSLL');

    NFSe.OutrasInformacoes                 := '';//Leitor.rCampo(tcStr, 'DescricaoRPS');
    NFSe.Servico.Discriminacao             := Leitor.rCampo(tcStr, 'DescricaoRPS');
    NFSe.Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoAtividade');

    NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'DDDPrestador') + Leitor.rCampo(tcStr, 'TelefonePrestador');
    NFSe.Tomador.Contato.Telefone          := Leitor.rCampo(tcStr, 'DDDTomador') + Leitor.rCampo(tcStr, 'TelefoneTomador');

    NFSE.MotivoCancelamento := Leitor.rCampo(tcStr, 'MotCancelamento');

    NFSe.IntermediarioServico.CpfCnpj := Leitor.rCampo(tcStr, 'CPFCNPJIntermediario');

    if (Leitor.rExtrai(1, 'Deducoes') <> '') then
    begin
      Item := 0;
      while (Leitor.rExtrai(1, 'Deducao', '', Item + 1) <> '') do
      begin
        FNfse.Servico.Deducao.Add;
        FNfse.Servico.Deducao[Item].DeducaoPor  :=
           StrToEnumerado( ok,Leitor.rCampo(tcStr, 'DeducaoPor'),
                           ['','Percentual','Valor'],
                           [ dpNenhum,dpPercentual, dpValor ]);

        FNfse.Servico.Deducao[Item].TipoDeducao :=
           StrToEnumerado( ok,Leitor.rCampo(tcStr, 'TipoDeducao'),
                           ['', 'Despesas com Materiais', 'Despesas com Sub-empreitada'],
                           [ tdNenhum, tdMateriais, tdSubEmpreitada ]);

        FNfse.Servico.Deducao[Item].CpfCnpjReferencia    := Leitor.rCampo(tcStr, 'CPFCNPJReferencia');
        FNfse.Servico.Deducao[Item].NumeroNFReferencia   := Leitor.rCampo(tcStr, 'NumeroNFReferencia');
        FNfse.Servico.Deducao[Item].ValorTotalReferencia := Leitor.rCampo(tcDe2, 'ValorTotalReferencia');
        FNfse.Servico.Deducao[Item].PercentualDeduzir    := Leitor.rCampo(tcDe2, 'PercentualDeduzir');
        FNfse.Servico.Deducao[Item].ValorDeduzir         := Leitor.rCampo(tcDe2, 'ValorDeduzir');
        inc(Item);
      end;
    end;

    if (Leitor.rExtrai(1, 'Itens') <> '') then
    begin
      Item := 0;
      while (Leitor.rExtrai(1, 'Item', '', Item + 1) <> '') do
      begin
        FNfse.Servico.ItemServico.Add;
        FNfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'DiscriminacaoServico');
        FNfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe2, 'Quantidade');
        FNfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
        FNfse.Servico.ItemServico[Item].ValorTotal    := Leitor.rCampo(tcDe2, 'ValorTotal');
        FNfse.Servico.ItemServico[Item].Tributavel    := StrToEnumerado( ok,Leitor.rCampo(tcStr, 'Tributavel'), ['N','S'], [ snNao, snSim ]);
        FNfse.Servico.Valores.ValorServicos           := (FNfse.Servico.Valores.ValorServicos + FNfse.Servico.ItemServico[Item].ValorTotal);
        inc(Item);
      end;
    end;
  end;

  (**** calculo anterior
  FNFSe.Servico.Valores.ValorLiquidoNfse := (FNfse.Servico.Valores.ValorServicos -
                                            (FNfse.Servico.Valores.ValorDeducoes +
                                             FNfse.Servico.Valores.DescontoCondicionado+
                                             FNfse.Servico.Valores.DescontoIncondicionado+
                                             FNFSe.Servico.Valores.ValorIssRetido));
  *)

  // Correção para o cálculo de VALOR LIQUIDO da NFSE - estavam faltando PIS, COFINS, INSS, IR e CSLL
  NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                            (NFSe.Servico.Valores.ValorPis +
                                             NFSe.Servico.Valores.ValorCofins +
                                             NFSe.Servico.Valores.ValorInss +
                                             NFSe.Servico.Valores.ValorIr +
                                             NFSe.Servico.Valores.ValorCsll +
                                             FNfse.Servico.Valores.ValorDeducoes +
                                             FNfse.Servico.Valores.DescontoCondicionado+
                                             FNfse.Servico.Valores.DescontoIncondicionado+
                                             FNFSe.Servico.Valores.ValorIssRetido);


  FNfse.Servico.Valores.BaseCalculo := NFSe.Servico.Valores.ValorLiquidoNfse;

  Result := True;
end;

function TNFSeR.LerNFSe_Smarapd: Boolean;
var
  vOk: Boolean;
  vItem: Integer;
  vLinha: String ;
begin
  Leitor.Grupo := Leitor.Arquivo;
  VersaoXML := '1';
  with FNFSe do
  begin
    Numero            := Leitor.rCampo(tcStr, 'NumeroNota');
    CodigoVerificacao := Leitor.rCampo(tcStr, 'ChaveValidacao');
    DataEmissaoRps    := Leitor.rCampo(tcDatHor, 'DataEmissao');
    Competencia       := Leitor.rCampo(tcStr, 'DataEmissao');
    DataEmissao       := Leitor.rCampo(tcDatHor, 'DataEmissao');
  end;
  with NFSe do
  begin
    Numero                  := Leitor.rCampo(tcStr, 'NumeroNota');
    CodigoVerificacao       := Leitor.rCampo(tcStr, 'ChaveValidacao');
    DataEmissaoRps          := Leitor.rCampo(tcDatHor, 'DataEmissao');
    Competencia             := Leitor.rCampo(tcStr, 'DataEmissao');
    DataEmissao             := Leitor.rCampo(tcDatHor, 'DataEmissao');
    if (Leitor.rCampo(tcStr, 'SituacaoNf') = 'Cancelada') then
    begin
      Status    := srCancelado;
      Cancelada := snSim;
      NfseCancelamento.DataHora:= Leitor.rCampo(tcDatHor, 'DataEmissao');
    end
    else
    begin
      Status    := srNormal;
      Cancelada := snNao;
    end;

    IdentificacaoRps.Numero := Leitor.rCampo(tcStr,'NumeroRps');
    IdentificacaoRps.Tipo   := trRPS;
    InfID.ID                := OnlyNumber(FNFSe.Numero);// + NFSe.IdentificacaoRps.Serie;
    with PrestadorServico do
    begin
     RazaoSocial                               := Trim(Leitor.rCampo(tcStr, 'TimbreContribuinteLinha1'));
     vLinha                                    := Trim(Leitor.rCampo(tcStr, 'TimbreContribuinteLinha2'));
     Endereco.Endereco                         := Trim(copy(vLinha,1,pos(',',vLinha)-1));
     Endereco.Numero                           := Trim(copy(vLinha,pos(',',vLinha)+1,(pos('-',vLinha)- pos(',',vLinha))-1));
     Endereco.Bairro                           := Trim(copy(vLinha,pos('-',vLinha)+1,length(vLinha)-1));
     Endereco.Complemento                      := '';
     vLinha                                    := Trim(Leitor.rCampo(tcStr, 'TimbreContribuinteLinha3'));
     vLinha                                    := Trim(copy(vLinha,pos('-',vLinha)+1,length(vLinha)-1));
     Endereco.xMunicipio                       := Trim(copy(vLinha,1,pos('-',vLinha)-1));
     Endereco.UF                               := Trim(copy(vLinha,pos('-',vLinha)+1,length(vLinha)-1));
     vLinha                                    := Trim(Leitor.rCampo(tcStr, 'TimbreContribuinteLinha4'));
     IdentificacaoPrestador.InscricaoMunicipal := Trim(copy(vLinha,23,(pos('CPF/CNPJ:',vLinha)-24)));
     IdentificacaoPrestador.Cnpj               := Trim(copy(vLinha,pos('CPF/CNPJ:',vLinha)+10,length(vLinha)-1));
     Contato.Telefone                          := '';
    end;
    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
       InscricaoMunicipal := Leitor.rCampo(tcStr, 'ClienteInscricaoMunicipal');
       CpfCnpj            := Leitor.rCampo(tcStr, 'ClienteCNPJCPF');
      end;
      RazaoSocial         := Leitor.rCampo(tcStr, 'ClienteNomeRazaoSocial');
      with Endereco do
      begin
        TipoLogradouro  := '';
        Endereco        := Leitor.rCampo(tcStr, 'ClienteEndereco');
        Numero          := Leitor.rCampo(tcStr, 'ClienteNumeroLogradouro');
        Complemento     := '';
        TipoBairro      := '';
        Bairro          := Leitor.rCampo(tcStr, 'ClienteBairro');
        CodigoMunicipio := '0';
        xMunicipio      := Leitor.rCampo(tcStr, 'ClienteCidade');
        UF              := Leitor.rCampo(tcStr, 'ClienteUF');
        CEP             := Leitor.rCampo(tcStr, 'ClienteCEP');
      end;
      with Contato do
      begin
        Email    := Leitor.rCampo(tcStr, 'ClienteEmail');
        Telefone := Leitor.rCampo(tcStr, 'ClienteFone') + Leitor.rCampo(tcStr, 'TelefoneTomador');
      end;
    end;
    with Servico do
    begin
      CodigoCnae := Leitor.rCampo(tcStr, 'CodigoAtividade');
      CodigoMunicipio := '';
      with Valores do
      begin
        Aliquota       := Leitor.rCampo(tcDe3, 'Aliquota');
        IssRetido      := StrToEnumerado( vOk, Leitor.rCampo(tcStr, 'ImpostoRetido'),['A','R'], [stNormal, stRetencao]);
        ValorPis       := Leitor.rCampo(tcDe2, 'Pis');
        ValorCofins    := Leitor.rCampo(tcDe2, 'Cofins');
        ValorInss      := Leitor.rCampo(tcDe2, 'Inss');
        ValorIr        := Leitor.rCampo(tcDe2, 'Irrf');
        ValorCsll      := Leitor.rCampo(tcDe2, 'Csll');
        AliquotaPIS    := 0;
        AliquotaCOFINS := 0;
        AliquotaINSS   := 0;
        AliquotaIR     := 0;
        AliquotaCSLL   := 0;
      end;
      OutrasInformacoes         := '';
      Discriminacao             := '';
      CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodigoAtividade');
    end;

    MotivoCancelamento           := '';
    IntermediarioServico.CpfCnpj := '';
    if (Leitor.rExtrai(1, 'ITENS') <> '') then
    begin
      vItem := 0 ;
      while (Leitor.rExtrai(1, 'ITENS', '', vItem + 1) <> '') do
      begin
        with  FNfse.Servico do
        begin
          ItemServico.Add;
          if NFSe.Servico.Discriminacao = '' then
            NFSe.Servico.Discriminacao := Leitor.rCampo(tcStr, 'Servico');
          ItemServico[vItem].Descricao     := Leitor.rCampo(tcStr, 'Servico');
          ItemServico[vItem].Quantidade    := Leitor.rCampo(tcStr, 'Quantidade');
          ItemServico[vItem].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorUnitario');
          ItemServico[vItem].ValorTotal    := Leitor.rCampo(tcDe2, 'ValorTotal');
          ItemServico[vItem].Tributavel    := snSim;
          ItemServico[vItem].Aliquota      := Leitor.rCampo(tcDe2, 'Aliquota');
          Valores.ValorServicos            := FNfse.Servico.Valores.ValorServicos + FNfse.Servico.ItemServico[vItem].ValorTotal;
          inc(vItem);
        end;
      end;
    end;
    FNfse.Servico.Valores.ValorIss        := (FNfse.Servico.Valores.ValorServicos * NFSe.Servico.Valores.Aliquota)/100;
    NFSe.Servico.Valores.ValorLiquidoNfse := FNfse.Servico.Valores.ValorServicos - (FNfse.Servico.Valores.ValorDeducoes +
                                             FNfse.Servico.Valores.DescontoCondicionado + FNfse.Servico.Valores.DescontoIncondicionado+
                                             NFSe.Servico.Valores.ValorIssRetido);
    FNfse.Servico.Valores.BaseCalculo     := NFSe.Servico.Valores.ValorLiquidoNfse;
  end;
 Result := True;
end;

function TNFSeR.LerNFSe_SP: Boolean;
var
  bOk :Boolean;
  valorIssRetido: Double;
begin
  Result := False;

  if (Leitor.rExtrai(1, 'NFe') <> '') or (Leitor.rExtrai(1, 'CompNfse') <> '') then
  begin
    NFSe.dhRecebimento  := Now;
    NFSe.Protocolo      := Leitor.rCampo(tcStr, 'NumeroLote');
    NFSe.NumeroLote     := Leitor.rCampo(tcStr, 'NumeroLote');
    NFSe.DataEmissao    := Leitor.rCampo(tcDatHor, 'DataEmissaoNFe');
    NFSe.DataEmissaoRps := Leitor.rCampo(tcDat, 'DataEmissaoRPS');

    if (Leitor.rCampo(tcStr, 'StatusNFe') = 'C') then
    begin
      NFSe.Status    := srCancelado;
      NFSe.Cancelada := snSim;
    end
    else
    begin
      NFSe.Status    := srNormal;
      NFSe.Cancelada := snNao;
    end;

    NFSe.TipoTributacaoRPS := StrToTTributacaoRPS(bOk, Leitor.rCampo(tcStr, 'TributacaoNFe'));

    if (Leitor.rCampo(tcStr, 'OpcaoSimples') = '0') then // ver pag do manual de integraçao...
      NFSe.OptanteSimplesNacional := snNao
    else
      NFSe.OptanteSimplesNacional := snSim;

    NFSe.ValoresNfse.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.ValoresNfse.BaseCalculo      := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.ValoresNfse.Aliquota         := Leitor.rCampo(tcDe2, 'AliquotaServicos');
    NFSe.ValoresNfse.ValorIss         := Leitor.rCampo(tcDe2, 'ValorISS');

    NFSe.Servico.ItemListaServico := Leitor.rCampo(tcStr, 'CodigoServico');
    NFSe.Servico.Discriminacao    := Leitor.rCampo(tcStr, 'Discriminacao');

    SetxItemListaServico;

    //NFSe.Servico.Valores.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.Servico.Valores.ValorServicos    := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.Servico.Valores.BaseCalculo      := Leitor.rCampo(tcDe2, 'ValorServicos');
    NFSe.Servico.Valores.Aliquota         := Leitor.rCampo(tcDe2, 'AliquotaServicos');
    NFSe.Servico.Valores.ValorIss         := Leitor.rCampo(tcDe2, 'ValorISS');

    // Tributos Federais - PIS, COFINS, INSS, IR e CSLL
    NFSe.Servico.Valores.ValorPis         := Leitor.rCampo(tcDe2, 'ValorPis');
    NFSe.Servico.Valores.ValorCofins      := Leitor.rCampo(tcDe2, 'ValorCofins');
    NFSe.Servico.Valores.ValorInss        := Leitor.rCampo(tcDe2, 'ValorInss');
    NFSe.Servico.Valores.ValorIr          := Leitor.rCampo(tcDe2, 'ValorIr');
    NFSe.Servico.Valores.ValorCsll        := Leitor.rCampo(tcDe2, 'ValorCsll');

    if (Leitor.rCampo(tcStr, 'ISSRetido') = 'false') then
    begin
      NFSe.Servico.Valores.IssRetido := stNormal;
      valorIssRetido := 0.00;
    end
    else
    begin
      NFSe.Servico.Valores.IssRetido := stRetencao;
      valorIssRetido := Leitor.rCampo(tcDe2, 'ValorISS');
    end;

    // Como o valor líquido não esta no layout deve refazer o cálculo
    (*
    NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos - (NFSe.Servico.Valores.ValorPis +
                                             NFSe.Servico.Valores.ValorCofins +
                                             NFSe.Servico.Valores.ValorInss +
                                             NFSe.Servico.Valores.ValorIr +
                                             NFSe.Servico.Valores.ValorCsll +
                                             valorIssRetido);
    *)

    NFSe.Servico.Valores.ValorLiquidoNfse := NFSe.Servico.Valores.ValorServicos -
                                              (NFSe.Servico.Valores.ValorPis +
                                               NFSe.Servico.Valores.ValorCofins +
                                               NFSe.Servico.Valores.ValorInss +
                                               NFSe.Servico.Valores.ValorIr +
                                               NFSe.Servico.Valores.ValorCsll +
                                               FNfse.Servico.Valores.ValorDeducoes +
                                               FNfse.Servico.Valores.DescontoCondicionado+
                                               FNfse.Servico.Valores.DescontoIncondicionado+
                                               FNFSe.Servico.Valores.ValorIssRetido);



    NFSe.PrestadorServico.RazaoSocial   := Leitor.rCampo(tcStr, 'RazaoSocialPrestador');
    NFSe.PrestadorServico.Contato.Email := Leitor.rCampo(tcStr, 'EmailPrestador');

    NFSe.Tomador.RazaoSocial   := Leitor.rCampo(tcStr, 'RazaoSocialTomador');
    NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'EmailTomador');

    if (Leitor.rExtrai(2, 'ChaveNFe') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoPrestador');
      NFSe.Numero            := Leitor.rCampo(tcStr, 'NumeroNFe');
      NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
    end;

    if (Leitor.rExtrai(2, 'ChaveRPS') <> '') then
    begin
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoPrestador');
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumeroRPS');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'SerieRPS');
      NFSe.IdentificacaoRps.Tipo   := trRPS;
      if NFSe.InfID.ID = '' then
        NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;

    if (Leitor.rExtrai(2, 'CPFCNPJPrestador') <> '') then
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'CNPJ');

    if (Leitor.rExtrai(2, 'EnderecoPrestador') <> '') then
    begin
      with NFSe.PrestadorServico.Endereco do
      begin
        TipoLogradouro  := Leitor.rCampo(tcStr, 'TipoLogradouro');
        Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
        Numero          := Leitor.rCampo(tcStr, 'NumeroEndereco');
        Complemento     := Leitor.rCampo(tcStr, 'ComplementoEndereco');
        Bairro          := Leitor.rCampo(tcStr, 'Bairro');
        CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
        UF              := Leitor.rCampo(tcStr, 'UF');
        CEP             := Leitor.rCampo(tcStr, 'CEP');
        xMunicipio      := CodCidadeToCidade(StrToIntDef(CodigoMunicipio, 0));
      end;
    end;

    with NFSe.Tomador do
    begin
      if (Leitor.rExtrai(2, 'CPFCNPJTomador') <> '') then
        IdentificacaoTomador.CpfCnpj := Leitor.rCampoCNPJCPF;

//      Contato.Email := Leitor.rCampo(tcStr, 'EmailTomador');

      if (Leitor.rExtrai(2, 'EnderecoTomador') <> '') then
      begin
        with Endereco do
        begin
          Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
          Numero          := Leitor.rCampo(tcStr, 'NumeroEndereco');
          Complemento     := Leitor.rCampo(tcStr, 'ComplementoEndereco');
          Bairro          := Leitor.rCampo(tcStr, 'Bairro');
          CodigoMunicipio := Leitor.rCampo(tcStr, 'Cidade');
          UF              := Leitor.rCampo(tcStr, 'UF');
          CEP             := Leitor.rCampo(tcStr, 'CEP');
          xMunicipio      := CodCidadeToCidade(StrToIntDef(CodigoMunicipio, 0));
        end;
      end;
    end;

    Result := True;
  end;
end;

function TNFSeR.LerNFSe_IPM: Boolean;
var
  I: Integer;
  vOk: Boolean;
  vItem: Integer;
  vLinha: String ;
begin
  Leitor.Grupo := Leitor.Arquivo;
  VersaoXML    := '1';

  if( Leitor.rExtrai( 1, 'nfse' ) <> '' )then
  begin
    if( Leitor.rExtrai( 2, 'rps' ) <> '' )then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo( tcStr, 'nro_recibo_provisorio' );
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo( tcStr, 'serie_recibo_provisorio' );
      NFSe.DataEmissaoRps          := StrToDateTimeDef(
                                        VarToStr(Leitor.rCampo( tcStr, 'data_emissao_recibo_provisorio' )) + ' ' +
                                        VarToStr(Leitor.rCampo( tcStr, 'hora_emissao_recibo_provisorio' )), 0 );
    end;

    if( Leitor.rExtrai( 2, 'nf' ) <> '' )then
    begin
      NFSe.Numero := Leitor.rCampo( tcStr, 'numero');

      // campos presentes ao baixar do site da prefeitura
      if (NFSe.Numero = '') then 
      begin
        NFSe.Numero         := Leitor.rCampo( tcStr, 'numero_nfse');
        NFSe.SeriePrestacao := Leitor.rCampo( tcStr, 'serie_nfse');
        NFSe.DataEmissao    := StrToDateTimeDef(
                                 VarToStr(Leitor.rCampo( tcStr, 'data_nfse' )) + ' ' +
                                 VarToStr(Leitor.rCampo( tcStr, 'hora_nfse' )), 0 );
      end;

      if Leitor.rCampo( tcStr, 'situacao' ) = 'C' then
      begin
        NFSe.Status := srCancelado;
        NFSe.Cancelada := snSim;
      end
      else
      begin
        NFSe.Status := srNormal;
        NFSe.Cancelada := snNao;
      end;

      NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo( tcDe2, 'valor_total' );
      NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo( tcDe2, 'valor_total' );
      NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo( tcDe2, 'valor_desconto' );
      NFSe.Servico.Valores.ValorIr                := Leitor.rCampo( tcDe2, 'valor_ir' );
      NFSe.Servico.Valores.ValorInss              := Leitor.rCampo( tcDe2, 'valor_inss' );
      NFSe.Servico.Valores.ValorCsll              := Leitor.rCampo( tcDe2, 'valor_contribuicao_social' );
      NFSe.Servico.Valores.ValorPis               := Leitor.rCampo( tcDe2, 'valor_pis' );
      NFSe.Servico.Valores.ValorCofins            := Leitor.rCampo( tcDe2, 'valor_cofins' );
      NFSe.OutrasInformacoes                      := Leitor.rCampo( tcStr, 'observacao' );
    end;

    if( Leitor.rExtrai( 2, 'prestador' ) <> '' )then
    begin
      NFSe.Prestador.Cnpj                               := Leitor.rCampo( tcStr, 'cpfcnpj' );
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo( tcStr, 'cpfcnpj' );
      NFSe.PrestadorServico.Endereco.CodigoMunicipio    := Leitor.rCampo( tcStr, 'cidade' );
    end;

    if( Leitor.rExtrai( 2, 'tomador' ) <> '' )then
    begin
      NFSe.Tomador.IdentificacaoTomador.CpfCnpj           := Leitor.rCampo( tcStr, 'cpfcnpj' );
      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := Leitor.rCampo( tcStr, 'ie' );

      NFSe.Tomador.RazaoSocial                  := Leitor.rCampo( tcStr, 'nome_razao_social' );
      NFSe.Tomador.Endereco.Endereco            := Leitor.rCampo( tcStr, 'logradouro' );
      NFSe.Tomador.Endereco.Numero              := Leitor.rCampo( tcStr, 'numero_residencia' );
      NFSe.Tomador.Endereco.Complemento         := Leitor.rCampo( tcStr, 'complemento' );
      NFSe.Tomador.Endereco.Bairro              := Leitor.rCampo( tcStr, 'bairro' );
      NFSe.Tomador.Endereco.CodigoMunicipio     := Leitor.rCampo( tcStr, 'cidade' );
      NFSe.Tomador.Endereco.CEP                 := Leitor.rCampo( tcStr, 'cep' );
      NFSe.Tomador.Contato.Email                := Leitor.rCampo( tcStr, 'email' );
      NFSe.Tomador.Contato.Telefone             := Leitor.rCampo( tcStr, 'ddd_fone_comercial' ) + Leitor.rCampo( tcStr, 'fone_comercial' );
    end;

    if( Leitor.rExtrai( 2, 'itens' ) <> '' )then
    begin
      I := 1;

      while( Leitor.rExtrai( 3, 'lista', 'lista', i ) <> '' )do
      begin
        with NFSe.Servico.ItemServico.Add do
        begin
          NFSe.NaturezaOperacao               := StrToNaturezaOperacao( vOk, IntToStr( AnsiIndexStr( Leitor.rCampo( tcStr, 'tributa_municipio_prestador' ), [ '1', '0' ] ) + 1 ) );
          NFSe.Servico.CodigoMunicipio        := Leitor.rCampo( tcStr, 'codigo_local_prestacao_servico' );
          Quantidade                          := Leitor.rCampo( tcDe3, 'unidade_quantidade' );
          ValorUnitario                       := Leitor.rCampo( tcDe2, 'unidade_valor_unitario' );
          NFSe.Servico.ItemListaServico       := PadLeft( Leitor.rCampo( tcStr, 'codigo_item_lista_servico' ), 4, '0' );
          Descricao                           := Leitor.rCampo( tcStr, 'descritivo' );
          Aliquota                            := Leitor.rCampo( tcDe2, 'aliquota_item_lista_servico');
          NFSe.Servico.Valores.IssRetido      := StrToSituacaoTributaria( vOk, IntToStr( AnsiIndexStr( Leitor.rCampo( tcStr, 'situacao_tributaria' ), [ '1', '0', '2' ] ) + 1 ) );
          ValorServicos                       := Leitor.rCampo( tcDe2, 'valor_tributavel');
          ValorDeducoes                       := Leitor.rCampo( tcDe2, 'valor_deducao');
          BaseCalculo                         := Leitor.rCampo( tcDe2, 'valor_tributavel');
          ValorIss                            := BaseCalculo * Aliquota / 100;
          NFSe.Servico.Valores.ValorIssRetido := NFSe.Servico.Valores.ValorIssRetido + Leitor.rCampo( tcDe2, 'valor_issrf');
          NFSe.Servico.Valores.BaseCalculo    := NFSe.Servico.Valores.BaseCalculo + BaseCalculo;
          NFSe.Servico.Valores.ValorIss       := NFSe.Servico.Valores.ValorIss + ValorIss;
        end;
        Inc( I );
      end;
    end;
  end;

  Result := True;
end;

function TNFSeR.LerNFSe_Infisc: Boolean;
begin
  Result := False;
  Leitor.Grupo := Leitor.Arquivo;

  if (Pos('<NFS-e>', Leitor.Arquivo) > 0) then
  begin
    if VersaoNFSe = ve110 then
      Result := LerNFSe_Infisc_V11
    else
      Result := LerNFSe_Infisc_V10;
  end;
end;

function TNFSeR.LerNFSe_EL: Boolean;
var
  ok: Boolean;
  I: integer;
  AValorTotal: Double;
begin
  Result := False;
  Leitor.Grupo := Leitor.Arquivo;

  if (Pos('<notasFiscais>', Leitor.Arquivo) > 0) or (Pos('<nfeRpsNotaFiscal>', Leitor.Arquivo) > 0) then
  begin
    NFSe.Numero                  := leitor.rCampo(tcStr, 'numero');
    NFSe.CodigoVerificacao       := leitor.rCampo(tcStr, 'idRps');
    NFSe.DataEmissao             := leitor.rCampo(tcDatHor, 'dataProcessamento');
    NFSe.IdentificacaoRps.Numero := leitor.rCampo(tcStr, 'rpsNumero');

    if Leitor.rExtrai(1, 'idNota') <> '' then
      NFSe.CodigoVerificacao := leitor.rCampo(tcStr, 'idNota');
      
    if (Leitor.rCampo(tcStr, 'situacao') <> 'A') then
    begin
      NFSe.Cancelada := snSim;
      NFSe.Status    := srCancelado;
    end;

    Result := True;
  end
  // loadfromfile de NF baixada manualmente
  else if (Pos('<Nfse>', Leitor.Arquivo) > 0) then
  begin
    NFSe.ChaveNFSe := Leitor.rCampo(tcStr, 'Id');

    NFSe.CodigoVerificacao := NFSe.ChaveNFSe;

    if (Leitor.rExtrai(2, 'IdentificacaoNfse') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    end;

    // Dados do prestador
    if (Leitor.rExtrai(2, 'DadosPrestador') <> '') then
    begin
      NFSe.NaturezaOperacao              := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
      NFSe.RegimeEspecialTributacao      := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
      NFSe.OptanteSimplesNacional        := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
      NFSe.IncentivadorCultural          := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));
      NFSe.PrestadorServico.RazaoSocial  := leitor.rCampo(tcStr, 'RazaoSocial');
      NFSe.PrestadorServico.NomeFantasia := leitor.rCampo(tcStr, 'NomeFantasia');

      if (Leitor.rExtrai(3, 'IdentificacaoPrestador') <> '') then
      begin
        NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj               := Leitor.rCampo(tcStr, 'CpfCnpj');
        NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
        NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');
      end;

      if (Leitor.rExtrai(3, 'Endereco') <> '') then
      begin
        with NFSe.PrestadorServico.Endereco do
        begin
          Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
          Numero          := Leitor.rCampo(tcStr, 'LogradouroNumero');
          Complemento     := Leitor.rCampo(tcStr, 'LogradouroComplemento');
          Bairro          := Leitor.rCampo(tcStr, 'Bairro');
          xMunicipio      := Leitor.rCampo(tcStr, 'Municipio');
          CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
          UF              := Leitor.rCampo(tcStr, 'Uf');
          CEP             := Leitor.rCampo(tcStr, 'Cep');
        end;
      end;

      if (Leitor.rExtrai(3, 'Contato') <> '') then
      begin
        NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
        NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;
    end; // fim Prestador


    // Dados do tomador
    if (Leitor.rExtrai(2, 'DadosTomador') <> '') then
    begin
      NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

      if (Leitor.rExtrai(3, 'IdentificacaoTomador') <> '') then
      begin
        NFSe.Tomador.IdentificacaoTomador.CpfCnpj            := Leitor.rCampo(tcStr, 'CpfCnpj');
        NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
        NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');
      end;

      if (Leitor.rExtrai(3, 'Endereco') <> '') then
      begin
        with NFSe.Tomador.Endereco do
        begin
          Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
          Numero          := Leitor.rCampo(tcStr, 'LogradouroNumero');
          Complemento     := Leitor.rCampo(tcStr, 'LogradouroComplemento');
          Bairro          := Leitor.rCampo(tcStr, 'Bairro');
          CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
          xMunicipio      := Leitor.rCampo(tcStr, 'Municipio');
          UF              := Leitor.rCampo(tcStr, 'Uf');
          CEP             := Leitor.rCampo(tcStr, 'Cep');
        end;
      end;

      if (Leitor.rExtrai(3, 'Contato') <> '' ) then
      begin
        NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
        NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;
    end; // fim Tomador

    // Dados dos Serviços
    if (Leitor.rExtrai(2, 'Servicos') <> '') then
    begin
      // Total máximo de 12 serviços na prefeitura
      for I := 1 to 12 do
      begin
        if (Leitor.rExtrai(3, 'Servicos', 'Servicos', I) <> '') then
        begin
          NFSe.Servico.ItemListaServico := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoServico116'));

          NFSe.Servico.ItemServico.Insert(I - 1);
          NFSe.Servico.ItemServico.Items[I - 1].CodServ       := Leitor.rCampo(tcStr, 'CodigoServico116');
          NFSe.Servico.ItemServico.Items[I - 1].CodLCServ     := Leitor.rCampo(tcStr, 'CodigoServico116');
          NFSe.Servico.ItemServico.Items[I - 1].Quantidade    := Leitor.rCampo(tcDe4, 'Quantidade');
          NFSe.Servico.ItemServico.Items[I - 1].Unidade       := Leitor.rCampo(tcStr, 'Unidade');
          NFSe.Servico.ItemServico.Items[I - 1].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorServico');
          NFSe.Servico.ItemServico.Items[I - 1].Descricao     := Leitor.rCampo(tcStr, 'Descricao');
          NFSe.Servico.ItemServico.Items[I - 1].Discriminacao := Leitor.rCampo(tcStr, 'Descricao');
          NFSe.Servico.ItemServico.Items[I - 1].Aliquota      := Leitor.rCampo(tcDe2, 'Aliquota');
          NFSe.Servico.ItemServico.Items[I - 1].ValorServicos := Leitor.rCampo(tcDe2, 'ValorServico');
          NFSe.Servico.ItemServico.Items[I - 1].ValorIss      := Leitor.rCampo(tcDe4, 'ValorIssqn');

          AValorTotal := NFSe.Servico.ItemServico.Items[I - 1].Quantidade *
                         NFSe.Servico.ItemServico.Items[I - 1].ValorUnitario;

          NFSe.Servico.ItemServico.Items[I - 1].ValorTotal    := RoundTo(AValorTotal, - 2);
        end 
        else
            Break;
      end;
    end; // fim Servicos

    if (Leitor.rExtrai(2, 'Valores') <> '') then
    begin
      with NFSe.Servico.Valores do
      begin
        ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
        ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
        ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
        ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
        ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
        ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
        ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
        ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
        ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
        OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
        ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
        OutrosDescontos        := Leitor.rCampo(tcDe2, 'OutrosDescontos');
        BaseCalculo            := ValorServicos - ValorDeducoes;
      end;
    end; // fim Valores

    // Outras Informações
    if (Leitor.rExtrai(2, 'Observacao') <> '') then
    begin
      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'Observacao');
    end;
                                 
    NFSe.Link := NFSe.ChaveNFSe; //ACBrNFSe1.LinkNFSe(StrToIntDef(NFSe.Numero, 0), NFSe.CodigoVerificacao, NFSe.ChaveNFSe);

    Result := True;
  end;
end;

function TNFSeR.LerNFSe_Equiplano: Boolean;
begin
  Result := False;
  Leitor.Grupo := Leitor.Arquivo;

  if (Pos('<nfse>', Leitor.Arquivo) > 0) or (Pos('<nfs', Leitor.Arquivo) > 0) then
  begin
    if (Pos('<nfse>', Leitor.Arquivo) > 0) then
    begin
      NFSe.Numero                  := leitor.rCampo(tcStr, 'nrNfse');
      NFSe.CodigoVerificacao       := leitor.rCampo(tcStr, 'cdAutenticacao');
      NFSe.DataEmissao             := leitor.rCampo(tcDatHor, 'dtEmissaoNfs');
      NFSe.IdentificacaoRps.Numero := leitor.rCampo(tcStr, 'nrRps');

      if (Leitor.rExtrai(1, 'cancelamento') <> '') or
         (Leitor.rExtrai(3, 'cancelamento') <> '') then
      begin
        NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'dtCancelamento');
        NFSe.MotivoCancelamento        := Leitor.rCampo(tcStr, 'dsCancelamento');
        NFSe.Status := srCancelado;
        NFSe.Cancelada := snSim;
      end;
    end
    else
    begin
      NFSe.Numero := Leitor.rCampo(tcStr, 'nrNfs');
      NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'cdAutenticacao');
      NFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'dtEmissaoNfs');
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'nrRps');
      NFSe.PrestadorServico.RazaoSocial := Leitor.rCampo(tcStr, 'nmPrestador');
      NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'nmPrestador');
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'dsEndereco');
      NFSe.PrestadorServico.Endereco.Numero := Leitor.rCampo(tcStr, 'nrEndereco');
      NFSe.PrestadorServico.Endereco.Bairro := Leitor.rCampo(tcStr, 'nmBairro');
      NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'nmUf');
      NFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'nrCEP');
      NFSe.PrestadorServico.Endereco.xMunicipio := Leitor.rCampo(tcStr, 'nmCidade');
      NFSe.PrestadorServico.Endereco.xPais := Leitor.rCampo(tcStr, 'nmPais');
      NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'nrDocumento');
      NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'nrInscricaoMunicipal');
      NFSe.Servico.Discriminacao := Leitor.rCampo(tcStr, 'dsDiscriminacaoServico');
      NFSe.Servico.Valores.ValorServicos := Leitor.rCampo(tcDe2, 'vlServico');
      NFSe.Servico.Valores.Aliquota := Leitor.rCampo(tcDe2, 'vlAliquota');
      NFSe.Servico.Valores.ValorISS := Leitor.rCampo(tcDe2, 'vlImposto');
      NFSe.Servico.Valores.BaseCalculo := Leitor.rCampo(tcDe2, 'vlBaseCalculo');

      if Leitor.rCampo(tcStr, 'isIssRetido') = 'Sim' then
        NFSe.Servico.Valores.ValorISSRetido := Leitor.rCampo(tcDe2, 'vlImposto');

      NFSe.Servico.Valores.ValorPIS := Leitor.rCampo(tcDe2, 'vlPis');
      NFSe.Servico.Valores.ValorCOFINS := Leitor.rCampo(tcDe2, 'vlCofins');
      NFSe.Servico.Valores.ValorIr := Leitor.rCampo(tcDe2, 'vlAliquotaIrpj');
      NFSe.Servico.Valores.ValorCSLL := Leitor.rCampo(tcDe2, 'vlCsll');
      NFSe.Servico.Valores.ValorINSS := Leitor.rCampo(tcDe2, 'vlInss');

      if Leitor.rExtrai(3, 'cancelamento') <> '' then
      begin
        NFSe.NfseCancelamento.DataHora := Leitor.rCampo(tcDatHor, 'dtCancelamento');
        NFSe.MotivoCancelamento := Leitor.rCampo(tcStr, 'dsCancelamento');
        NFSe.Status := srCancelado;
      end;
    end;

    if (Leitor.rExtrai(1, 'tomadorServico') <> '') then
    begin
      NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'nmTomador');
      NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'nrDocumento');
      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'dsEndereco');
      NFSe.Tomador.Endereco.Numero := Leitor.rCampo(tcStr, 'nrEndereco');
      NFSe.Tomador.Endereco.xPais := Leitor.rCampo(tcStr, 'nmPais');
      NFSe.Tomador.Endereco.xMunicipio := Leitor.rCampo(tcStr, 'nmCidade');
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'cdIbge');
      NFSe.Tomador.Endereco.Bairro := Leitor.rCampo(tcStr, 'nmBairro');
      NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'nmUf');
      NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'nrCep');
    end;

    Result := True;
  end;
end;

function TNFSeR.LerRps_EL: Boolean;
var
 ok  : Boolean;
  I: Integer;
  AValorTotal: Double;
begin
  if (Leitor.rExtrai(1, 'Rps') <> '') then
  begin
    NFSe.InfID.ID    := Leitor.rCampo(tcStr, 'Id');
    NFSe.DataEmissao := Leitor.rCampo(tcDatHor, 'DataEmissao');
    NFSe.Status      := StrToStatusRPS(ok, Leitor.rCampo(tcStr, 'Status'));

    if (Leitor.rExtrai(2, 'IdentificacaoRps') <> '') then
    begin
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      NFSe.IdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      NFSe.IdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
    end;

    // Dados do prestador
    if (Leitor.rExtrai(2, 'DadosPrestador') <> '') then
    begin
      NFSe.NaturezaOperacao              := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'NaturezaOperacao'));
      NFSe.RegimeEspecialTributacao      := StrToRegimeEspecialTributacao(ok, Leitor.rCampo(tcStr, 'RegimeEspecialTributacao'));
      NFSe.OptanteSimplesNacional        := StrToSimNao(ok, Leitor.rCampo(tcStr, 'OptanteSimplesNacional'));
      NFSe.IncentivadorCultural          := StrToSimNao(ok, Leitor.rCampo(tcStr, 'IncentivadorCultural'));
      NFSe.PrestadorServico.RazaoSocial  := leitor.rCampo(tcStr, 'RazaoSocial');
      NFSe.PrestadorServico.NomeFantasia := leitor.rCampo(tcStr, 'NomeFantasia');

      if (Leitor.rExtrai(3, 'IdentificacaoPrestador') <> '') then
      begin
        NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj               := Leitor.rCampo(tcStr, 'CpfCnpj');
        NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
        NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');
      end;

      if (Leitor.rExtrai(3, 'Endereco') <> '') then
      begin
        with NFSe.PrestadorServico.Endereco do
        begin
          Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
          Numero          := Leitor.rCampo(tcStr, 'LogradouroNumero');
          Complemento     := Leitor.rCampo(tcStr, 'LogradouroComplemento');
          Bairro          := Leitor.rCampo(tcStr, 'Bairro');
          xMunicipio      := Leitor.rCampo(tcStr, 'Municipio');
          CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
          UF              := Leitor.rCampo(tcStr, 'Uf');
          CEP             := Leitor.rCampo(tcStr, 'Cep');
        end;
      end;

      if (Leitor.rExtrai(3, 'Contato') <> '') then
      begin
        NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
        NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;
    end; // fim Prestador

    // Dados do tomador
    if (Leitor.rExtrai(2, 'DadosTomador') <> '') then
    begin
      NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazaoSocial');

      if (Leitor.rExtrai(3, 'IdentificacaoTomador') <> '') then
      begin
        NFSe.Tomador.IdentificacaoTomador.CpfCnpj            := Leitor.rCampo(tcStr, 'CpfCnpj');
        NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'InscricaoMunicipal');
        NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual  := Leitor.rCampo(tcStr, 'InscricaoEstadual');
      end;

      if (Leitor.rExtrai(3, 'Endereco') <> '') then
      begin
        with NFSe.Tomador.Endereco do
        begin
          Endereco        := Leitor.rCampo(tcStr, 'Logradouro');
          Numero          := Leitor.rCampo(tcStr, 'LogradouroNumero');
          Complemento     := Leitor.rCampo(tcStr, 'LogradouroComplemento');
          Bairro          := Leitor.rCampo(tcStr, 'Bairro');
          CodigoMunicipio := Leitor.rCampo(tcStr, 'CodigoMunicipio');
          xMunicipio      := Leitor.rCampo(tcStr, 'Municipio');
          UF              := Leitor.rCampo(tcStr, 'Uf');
          CEP             := Leitor.rCampo(tcStr, 'Cep');
        end;
      end;

      if (Leitor.rExtrai(3, 'Contato') <> '' ) then
      begin
        NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'Telefone');
        NFSe.Tomador.Contato.Email    := Leitor.rCampo(tcStr, 'Email');
      end;
    end; // fim Tomador

    // Dados dos Serviços
    if (Leitor.rExtrai(2, 'Servicos') <> '') then
    begin
      // Total máximo de 12 serviços na prefeitura
      for I := 1 to 12 do
      begin
        if (Leitor.rExtrai(3, 'Servico', 'Servico', I) <> '') then
        begin
          NFSe.Servico.ItemListaServico := OnlyNumber(Leitor.rCampo(tcStr, 'CodigoServico116'));

          NFSe.Servico.ItemServico.Insert(I - 1);
          NFSe.Servico.ItemServico.Items[I - 1].CodServ       := Leitor.rCampo(tcStr, 'CodigoServico116');
          NFSe.Servico.ItemServico.Items[I - 1].CodLCServ     := Leitor.rCampo(tcStr, 'CodigoServico116');
          NFSe.Servico.ItemServico.Items[I - 1].Quantidade    := Leitor.rCampo(tcDe4, 'Quantidade');
          NFSe.Servico.ItemServico.Items[I - 1].Unidade       := Leitor.rCampo(tcStr, 'Unidade');
          NFSe.Servico.ItemServico.Items[I - 1].ValorUnitario := Leitor.rCampo(tcDe2, 'ValorServico');
          NFSe.Servico.ItemServico.Items[I - 1].Descricao     := Leitor.rCampo(tcStr, 'Descricao');
          NFSe.Servico.ItemServico.Items[I - 1].Aliquota      := Leitor.rCampo(tcDe2, 'Aliquota');
          NFSe.Servico.ItemServico.Items[I - 1].ValorServicos := Leitor.rCampo(tcDe2, 'ValorServico');
          NFSe.Servico.ItemServico.Items[I - 1].ValorIss      := Leitor.rCampo(tcDe4, 'ValorIssqn');

          AValorTotal := NFSe.Servico.ItemServico.Items[I - 1].Quantidade *
                         NFSe.Servico.ItemServico.Items[I - 1].ValorUnitario;

          NFSe.Servico.ItemServico.Items[I - 1].ValorTotal    := RoundTo(AValorTotal, - 2);
        end 
	    else
          Break;
      end;
    end; // fim Servicos

    if (Leitor.rExtrai(2, 'Valores') <> '') then
    begin
      with NFSe.Servico.Valores do
      begin
        ValorServicos          := Leitor.rCampo(tcDe2, 'ValorServicos');
        ValorIss               := Leitor.rCampo(tcDe2, 'ValorIss');
        ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'ValorLiquidoNfse');
        ValorDeducoes          := Leitor.rCampo(tcDe2, 'ValorDeducoes');
        ValorPis               := Leitor.rCampo(tcDe2, 'ValorPis');
        ValorCofins            := Leitor.rCampo(tcDe2, 'ValorCofins');
        ValorInss              := Leitor.rCampo(tcDe2, 'ValorInss');
        ValorIr                := Leitor.rCampo(tcDe2, 'ValorIr');
        ValorCsll              := Leitor.rCampo(tcDe2, 'ValorCsll');
        OutrasRetencoes        := Leitor.rCampo(tcDe2, 'OutrasRetencoes');
        ValorIssRetido         := Leitor.rCampo(tcDe2, 'ValorIssRetido');
        OutrosDescontos        := Leitor.rCampo(tcDe2, 'OutrosDescontos');
        BaseCalculo            := ValorServicos - ValorDeducoes;
      end;
    end; // fim Valores

    // Outras Informações
    if (Leitor.rExtrai(2, 'Observacao') <> '') then
    begin
      NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'Observacao');
    end;
  end; // fim Rps

  Result := True;
end;

function TNFSeR.LerNFSe_Governa: Boolean;
var
  i: integer;
begin
  NFSe.dhRecebimento                := StrToDateTime(formatdatetime ('dd/mm/yyyy',now));
  NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'CodCadBic');
  NFSe.Prestador.ChaveAcesso        := Leitor.rCampo(tcStr, 'ChvAcs');
  NFSe.Numero                       := Leitor.rCampo(tcStr, 'NumNot');
  NFSe.IdentificacaoRps.Numero      := Leitor.rCampo(tcStr, 'NumRps');

  if (Leitor.rExtrai(1, 'Nfse') <> '') then
  begin
    with NFSe do
    begin
      Numero := Leitor.rCampo(tcStr, 'NumNot');
      NFSe.IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumRps');
      NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodVer');
      PrestadorServico.RazaoSocial := Leitor.rCampo(tcStr, 'RzSocialPr');
      PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'CNPJPr');
      PrestadorServico.IdentificacaoPrestador.InscricaoEstadual := Leitor.rCampo(tcStr, 'IEPr');
      PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'CodCadBic');
      PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'EndLogradouroPr');
      PrestadorServico.Endereco.Numero :=  Leitor.rCampo(tcStr, 'EndNumeroPr');
      PrestadorServico.Endereco.Bairro := Leitor.rCampo(tcStr, 'EndBairroPr');
      PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'EndComplPr');
      PrestadorServico.Endereco.xMunicipio := Leitor.rCampo(tcStr, 'EndCidadePr');
      PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'EndCEPPr');
      PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'EndUFPr');
      Servico.Valores.ValorServicos := Leitor.rCampo(tcStr, 'VlrUnt');
      Servico.Valores.ValorPis := Leitor.rCampo(tcStr, 'VlrPIS');
      Servico.Valores.ValorCofins := Leitor.rCampo(tcStr, 'VlrCofins');
      Servico.Valores.ValorCofins := Leitor.rCampo(tcStr, 'VlrINSS');
      Servico.Valores.ValorInss := Leitor.rCampo(tcStr, 'VlrIR');
      DataEmissao := Leitor.rCampo(tcDat, 'DtemiNfse');
      Nfse.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'NomTmd');
      NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'NumDocTmd');
      NFSe.TipoRecolhimento := Leitor.rCampo(tcStr, 'TipRec');
      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := Leitor.rCampo(tcStr, 'InscricaoEstadual');
      Nfse.OutrasInformacoes := Leitor.rCampo(tcStr, 'Obs');
      with  Nfse.Tomador.Endereco do
      begin
        Endereco := Leitor.rCampo(tcStr, 'DesEndTmd');
        Bairro := Leitor.rCampo(tcStr, 'NomBaiTmd');
        xMunicipio := Leitor.rCampo(tcStr, 'NomCidTmd');
        UF := Leitor.rCampo(tcStr, 'CodEstTmd');
        CEP := Leitor.rCampo(tcStr, 'CEPTmd');
      end;

      Competencia := Leitor.rCampo(tcStr, 'DtemiNfse');
      Servico.CodigoCnae := Leitor.rCampo(tcStr, 'CodAti');
      Servico.Discriminacao := Leitor.rCampo(tcStr, 'DesSvc');
      Servico.Descricao := Leitor.rCampo(tcStr, 'DescricaoServ');

//    Itens do serviço prestado
      i := 0;
      while i <> -1 do
      begin
        if (Leitor.rExtrai(1, 'ItemNfse','',i+1) <> '') then
        begin
          Nfse.Servico.ItemServico.Insert(i);
          with NFSe.Servico.ItemServico.Items[i] do
          begin
            Descricao := Leitor.rCampo(tcStr, 'DesSvc');
            Quantidade := StrToFloat(Leitor.rCampo(tcStr, 'QdeSvc'));
            ValorUnitario := StrToFloat(Leitor.rCampo(tcStr, 'VlrUnt'));
            ValorTotal := StrToFloat(Leitor.rCampo(tcStr, 'QdeSvc')) * StrToFloat(Leitor.rCampo(tcStr, 'VlrUnt'));
          end;
          inc(i);
        end
        else
        begin
          i := -1;
        end;
      end;

      for i := 0 to Nfse.Servico.ItemServico.Count - 1 do
      begin
        Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorLiquidoNfse + NFse.Servico.ItemServico.Items[i].ValorTotal;
      end;
    end;
  end;

  Result := True;
end;

function TNFSeR.LerNFSe_CONAM: Boolean;
var
  i: Integer;
  bNota: Boolean;
  bRPS: Boolean;
  sDataTemp: String;
  ValorIssRet: Double;  //Edson
begin
  bRPS := False;
  bNota := False;

  if (Leitor.rExtrai(1, 'Reg20Item') <> '') then
    bRPS := True
  else
    if (Leitor.rExtrai(1, 'CompNfse') <> '') then
      bNota := True;

  if bNota or bRPS then
  begin
    with NFSe do
    begin
      Numero := Leitor.rCampo(tcStr, 'NumNf');
      SeriePrestacao := Leitor.rCampo(tcStr, 'SerNf');
      sDataTemp := Leitor.rCampo(tcStr, 'DtEmi');

      if sDataTemp = EmptyStr then
        sDataTemp := Leitor.rCampo(tcStr, 'DtEmiNf');

      if sDataTemp <> EmptyStr then
      begin
        DataEmissao := StrToDate(sDataTemp);
        Competencia := FormatDateTime('mm/yyyy', StrToDate(sDataTemp));
      end;

      sDataTemp := Leitor.rCampo(tcStr, 'DtEmiRps');
      DataEmissaoRps := StrToDate(sDataTemp);

      sDataTemp := Leitor.rCampo(tcStr, 'DtHrGerNf');
      dhRecebimento := StrToDateTimeDef(sDataTemp, Now);

      IdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'NumRps');
      IdentificacaoRps.Serie:= Leitor.rCampo(tcStr, 'SerRps');
      CodigoVerificacao := Leitor.rCampo(tcStr, 'CodVernf');

      ValorIssRet := Leitor.rCampo(tcDe2, 'VlIssRet');  //Edson

      if Leitor.rCampo(tcStr, 'TipoTribPre') = 'SN' then
        OptanteSimplesNacional := snSim
      else
        OptanteSimplesNacional := snNao;

      sDataTemp := Leitor.rCampo(tcStr, 'DtAdeSN');
      if (sDataTemp <> EmptyStr) and (sDataTemp <> '/  /') then
        DataOptanteSimplesNacional := StrToDate(sDataTemp);

      PrestadorServico.RazaoSocial := Leitor.rCampo(tcStr, 'RazSocPre');
      PrestadorServico.IdentificacaoPrestador.Cnpj := Leitor.rCampo(tcStr, 'CpfCnpjPre');
      PrestadorServico.IdentificacaoPrestador.InscricaoEstadual := Leitor.rCampo(tcStr, 'IEPr');
      PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'CodCadBic');
      PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'LogPre');
      PrestadorServico.Endereco.Numero :=  Leitor.rCampo(tcStr, 'NumEndPre');
      PrestadorServico.Endereco.Bairro := Leitor.rCampo(tcStr, 'BairroPre');
      PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'ComplEndPre');
      PrestadorServico.Endereco.xMunicipio := Leitor.rCampo(tcStr, 'MunPre');
      PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'CepPre');
      PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'SiglaUFPre');

      ValoresNfse.ValorLiquidoNfse := Leitor.rCampo(tcDe2, 'VlNFS');
      ValoresNfse.Aliquota := Leitor.rCampo(tcDe2, 'AlqIss');
      ValoresNfse.ValorIss := Leitor.rCampo(tcDe2, 'VlIss');
      ValoresNfse.BaseCalculo := Leitor.rCampo(tcDe2, 'VlBasCalc');

      Servico.Valores.BaseCalculo := Leitor.rCampo(tcDe2, 'VlBasCalc');
      Servico.Valores.ValorServicos := Leitor.rCampo(tcDe2, 'VlNFS');
      Servico.Valores.Aliquota := Leitor.rCampo(tcDe2, 'AlqIss');
      Servico.Valores.IssRetido := Leitor.rCampo(tcDe2, 'VlIssRet'); // Isto nao está funcionando...
              //O conteúdo de "Servico.Valores.IssRetido dever ser SIM ou NAO
              //O conteúdo de "Sevrico.Valores.ValorIssRetido é o valor do ISS
      Servico.Valores.ValorDeducoes := Leitor.rCampo(tcDe2, 'VlDed');
      Servico.Valores.JustificativaDeducao := Leitor.rCampo(tcStr, 'DiscrDed');

      if ValorIssRet>0 then
      begin
          Servico.Valores.IssRetido      := stRetencao;   //Edson
          Servico.Valores.ValorIssRetido := ValorIssRet;  //Edson
          Servico.Valores.ValorIss       := 0;            //Edson
          Servico.Valores.ValorLiquidoNfse:= Servico.Valores.ValorServicos - ValorIssRet;   //Edson
          ValoresNfse.ValorLiquidoNfse    := Servico.Valores.ValorServicos - ValorIssRet;   //Edson
      end else
      begin
          Servico.Valores.IssRetido      := stNormal;    //Edson
          Servico.Valores.ValorIssRetido := 0;           //Edson
          Servico.Valores.ValorIss       := Leitor.rCampo(tcDe2, 'VlIss');    //Edson
          Servico.Valores.ValorLiquidoNfse:= Leitor.rCampo(tcDe2, 'VlNFS');   //Edson
          ValoresNfse.ValorLiquidoNfse    := Leitor.rCampo(tcDe2, 'VlNFS');   //Edson
      end;

      Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'RazSocTom');
      Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampo(tcStr, 'CpfCnpjTom');
      with  Tomador.Endereco do
      begin
        Endereco := Leitor.rCampo(tcStr, 'LogTom');
        Numero := Leitor.rCampo(tcStr, 'NumEndTom');;
        Complemento := Leitor.rCampo(tcStr, 'ComplEndTom');
        Bairro := Leitor.rCampo(tcStr, 'BairroTom');
        xMunicipio := Leitor.rCampo(tcStr, 'MunTom');
        UF := Leitor.rCampo(tcStr, 'SiglaUFTom');
        CEP := Leitor.rCampo(tcStr, 'CepTom');
      end;

//      Servico.CodigoTributacaoMunicipio := Leitor.rCampo(tcStr, 'CodSrv');
      Servico.ItemListaServico := Leitor.rCampo(tcStr, 'CodSrv');

      if TabServicosExt then
        Servico.xItemListaServico := ObterDescricaoServico(OnlyNumber(Servico.ItemListaServico))
      else
        Servico.xItemListaServico := CodigoToDesc(OnlyNumber(Servico.ItemListaServico));
      {
       todo: almp1
       deveria substrituir "\\" por "nova linha"
      }
      Servico.Discriminacao := Leitor.rCampo(tcStr, 'DiscrSrv');
      Situacao := Leitor.rCampo(tcStr, 'SitNf');
      MotivoCancelamento := Leitor.rCampo(tcStr, 'MotivoCncNf');

      //valores dos tributos
      if (Leitor.rExtrai(1, 'Reg30') <> '') then
      begin
        i := 1;
        while (Leitor.rExtrai(1, 'Reg30Item', '', i) <> '') do
        begin
          if Leitor.rCampo(tcStr, 'TributoSigla') = 'IR' then
          begin
            Servico.Valores.AliquotaIr := Leitor.rCampo(tcDe2, 'TributoAliquota');
            Servico.Valores.ValorIr := Leitor.rCampo(tcDe2, 'TributoValor');
          end;
          if Leitor.rCampo(tcStr, 'TributoSigla') = 'PIS' then
          begin
            Servico.Valores.AliquotaPis := Leitor.rCampo(tcDe2, 'TributoAliquota');
            Servico.Valores.ValorPis := Leitor.rCampo(tcDe2, 'TributoValor');
          end;
          if Leitor.rCampo(tcStr, 'TributoSigla') = 'COFINS' then
          begin
            Servico.Valores.AliquotaCofins := Leitor.rCampo(tcDe2, 'TributoAliquota');
            Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'TributoValor');
          end;
          if Leitor.rCampo(tcStr, 'TributoSigla') = 'CSLL' then
          begin
            Servico.Valores.AliquotaCsll := Leitor.rCampo(tcDe2, 'TributoAliquota');
            Servico.Valores.ValorCsll := Leitor.rCampo(tcDe2, 'TributoValor');
          end;
          if Leitor.rCampo(tcStr, 'TributoSigla') = 'INSS' then
          begin
            Servico.Valores.AliquotaInss := Leitor.rCampo(tcDe2, 'TributoAliquota');
            Servico.Valores.ValorInss := Leitor.rCampo(tcDe2, 'TributoValor');
          end;
          Inc(i);
        end;
      end;

      InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;
    end;
  end;

  Result := True;
end;

function TNFSeR.LerNFSe_Infisc_V10: Boolean;
var
  ok: Boolean;
  dEmi: String;
  hEmi: String;
  dia, mes, ano, hora, minuto: Word;
  Item: Integer;
begin
  VersaoXML:= '1';

  if (Leitor.rExtrai(1, 'Id') <> '') then
  begin
    NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'cNFS-e');
    NFSe.NaturezaOperacao  := StrToNaturezaOperacao(ok, Leitor.rCampo(tcStr, 'natOp'));
    NFSe.SeriePrestacao    := Leitor.rCampo(tcStr, 'serie');
    NFSe.Numero            := Leitor.rCampo(tcStr, 'nNFS-e');
    NFSe.Competencia       := Leitor.rCampo(tcStr, 'dEmi');

    dEmi := NFSe.Competencia;
    hEmi := Leitor.rCampo(tcStr, 'hEmi');

    ano := StrToInt( copy( dEmi, 1 , 4 ) );
    mes := strToInt( copy( dEmi, 6 , 2 ) );
    dia := strToInt( copy( dEmi, 9 , 2 ) );

    hora   := strToInt( Copy( hEmi , 0 , 2 ) );
    minuto := strToInt( copy( hEmi , 4 , 2 ) );

    Nfse.DataEmissao := EncodeDateTime( ano, mes, dia, hora, minuto, 0, 0);

    NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'cMunFG');
    NFSe.ChaveNFSe               := Leitor.rCampo(tcStr, 'refNF');

    NFSe.Status      := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'anulada'), ['N','S'], [srNormal, srCancelado]);
    NFSe.InfID.ID    := OnlyNumber(NFSe.CodigoVerificacao);

    NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Servico.CodigoMunicipio, 0);
  end;

  if (Leitor.rExtrai(1, 'emit') <> '') then
  begin
    NFSe.Prestador.Cnpj := Leitor.rCampo(tcStr, 'CNPJ');
    NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := NFSe.Prestador.Cnpj;
    NFSe.PrestadorServico.RazaoSocial := Leitor.rCampo(tcStr, 'xNome');
    NFSe.Prestador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'IM');
    NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSe.Prestador.InscricaoMunicipal;
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'xFant');

    if (Leitor.rExtrai(2, 'end') <> '') then
    begin
      NFSe.PrestadorServico.Endereco.Endereco := Leitor.rCampo(tcStr, 'xLgr');
      NFSe.PrestadorServico.Endereco.Numero := Leitor.rCampo(tcStr, 'nro');
      NFSe.PrestadorServico.Endereco.Complemento := Leitor.rCampo(tcStr, 'xCpl');
      NFSe.PrestadorServico.Endereco.Bairro := Leitor.rCampo(tcStr, 'xBairro');
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'cMun');
      NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
      NFSe.PrestadorServico.Endereco.UF := Leitor.rCampo(tcStr, 'UF');
      NFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'CEP');
      NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'fone');
      NFSe.PrestadorServico.Contato.Email := Leitor.rCampo(tcStr, 'xEmail');
    end;

  end;

  if (Leitor.rExtrai(1, 'TomS') <> '') then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampoCNPJCPF;
    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'xNome');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'IM');

    if (Leitor.rExtrai(2, 'ender') <> '') then
    begin
      NFSe.Tomador.Endereco.Endereco := Leitor.rCampo(tcStr, 'xLgr');
      NFSe.Tomador.Endereco.Numero := Leitor.rCampo(tcStr, 'nro');
      NFSe.Tomador.Endereco.Complemento := Leitor.rCampo(tcStr, 'xCpl');
      NFSe.Tomador.Endereco.Bairro := Leitor.rCampo(tcStr, 'xBairro');
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'cMun');
      NFSe.Tomador.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
      NFSe.Tomador.Endereco.UF := Leitor.rCampo(tcStr, 'UF');
      NFSe.Tomador.Endereco.CEP := Leitor.rCampo(tcStr, 'CEP');
      NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'fone');
      NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'xEmail');
    end;

  end;

  // Detalhes dos serviços
  Item := 0;
  while (Leitor.rExtrai(1, 'det', '', Item + 1) <> '') do
  begin

    if Leitor.rExtrai(2, 'serv') <> '' then
    begin
      Nfse.Servico.ItemServico.Add;
      Nfse.Servico.ItemServico[Item].Codigo        := Leitor.rCampo(tcStr, 'cServ');
      Nfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'xServ');
      Nfse.Servico.ItemServico[Item].Discriminacao := FNfse.Servico.ItemServico[Item].Codigo+' - '+FNfse.Servico.ItemServico[Item].Descricao;
      Nfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe4, 'qTrib');
      Nfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe3, 'vUnit');
      Nfse.Servico.ItemServico[Item].ValorServicos := Leitor.rCampo(tcDe2, 'vServ');
      Nfse.Servico.ItemServico[Item].DescontoIncondicionado := Leitor.rCampo(tcDe2, 'vDesc');
      // ISSQN
      Nfse.Servico.ItemServico[Item].BaseCalculo := Leitor.rCampo(tcDe2, 'vBCISS');
      Nfse.Servico.ItemServico[Item].Aliquota    := Leitor.rCampo(tcDe2, 'pISS');
      Nfse.Servico.ItemServico[Item].ValorIss    := Leitor.rCampo(tcDe2, 'vISS');
      // Retenções
      NFSe.Servico.ItemServico.Items[Item].ValorIr     := Leitor.rCampo(tcDe2, 'vRetIRF');
      NFSe.Servico.ItemServico.Items[Item].ValorPis    := Leitor.rCampo(tcDe2, 'vRetLei10833-PIS-PASEP');
      NFSe.Servico.ItemServico.Items[Item].ValorCofins := Leitor.rCampo(tcDe2, 'vRetLei10833-COFINS');
      NFSe.Servico.ItemServico.Items[Item].ValorCsll   := Leitor.rCampo(tcDe2, 'vRetLei10833-CSLL');
      NFSe.Servico.ItemServico.Items[Item].ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
    end;

    inc(Item);
  end;

  if (Leitor.rExtrai(1, 'total') <> '') then
  begin
    // Valores
    NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'vServ');
    NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'vDesc');
    NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'vtLiq');

    if (Leitor.rExtrai(2, 'fat') <> '') then
    begin
      // Fatura
    end;

    if (Leitor.rExtrai(2, 'ISS') <> '') then
    begin
      // ISSQN
      NFSe.Servico.Valores.BaseCalculo := Leitor.rCampo(tcDe2, 'vBCISS');
      NFSe.Servico.Valores.ValorIss    := Leitor.rCampo(tcDe2, 'vISS');
      // ISSQN Retido
      NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'vSTISS');

      if NFSe.Servico.Valores.ValorIssRetido > 0 then
      begin
        NFSe.Servico.Valores.IssRetido   := stRetencao;
        NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0);
      end;

    end;

    (*
        // Retenções
        NFSe.Servico.Valores.ValorIr     := Leitor.rCampo(tcDe2, 'vRetIRF');
        NFSe.Servico.Valores.ValorPis    := Leitor.rCampo(tcDe2, 'vRetLei10833-PIS-PASEP');
        NFSe.Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'vRetLei10833-COFINS');
        NFSe.Servico.Valores.ValorCsll   := Leitor.rCampo(tcDe2, 'vRetLei10833-CSLL');
        NFSe.Servico.Valores.ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
    *)

  end;

  if (Leitor.rExtrai(1, 'cobr') <> '') then
  begin
    // Cobrança
  end;

  if (Leitor.rExtrai(1, 'Observacoes') <> '') then
  begin
    NFSe.OutrasInformacoes := Leitor.rCampo(tcStr, 'xInf');
  end;

  if (Leitor.rExtrai(1, 'reemb') <> '') then
  begin
    // reembolso
  end;

  if (Leitor.rExtrai(1, 'ISSST') <> '') then
  begin
    // ISS Substituição Tributária
  end;

  (*
      // Lay-Out Infisc não possui campo específicos
      NFSe.Servico.ItemListaServico := Leitor.rCampo(tcStr, 'infAdic');
  *)

  Result := True;
end;

function TNFSeR.LerNFSe_Infisc_V11: Boolean;
var
  ok: Boolean;
  dEmi: String;
  hEmi: String;
  dia, mes, ano, hora, minuto: Word;
  Item: Integer;
(*
  lIndex: Integer;
  lTextoAposInfAdic: String;
*)
begin
  VersaoXML:= '1.1';

  NFSe.Servico.CodigoMunicipio := Leitor.rCampo(tcStr, 'infAdicLT');

  if (Leitor.rExtrai(1, 'Id') <> '') then
  begin
    NFSe.Numero            := Leitor.rCampo(tcStr, 'nNFS-e');
    NFSe.CodigoVerificacao := Leitor.rCampo(tcStr, 'cNFS-e');
    NFSe.SeriePrestacao    := Leitor.rCampo(tcStr, 'serie');
    NFSe.Competencia       := Leitor.rCampo(tcStr, 'dEmi');
    NFSe.ModeloNFSe        := Leitor.rCampo(tcStr, 'mod');
    dEmi := NFSe.Competencia;
    hEmi := Leitor.rCampo(tcStr, 'hEmi');
    ano := StrToInt( copy( dEmi, 1 , 4 ) );
    mes := strToInt( copy( dEmi, 6 , 2 ) );
    dia := strToInt( copy( dEmi, 9 , 2 ) );
    hora   := strToInt( Copy( hEmi , 0 , 2 ) );
    minuto := strToInt( copy( hEmi , 4 , 2 ) );
    Nfse.DataEmissao := EncodeDateTime( ano, mes, dia, hora, minuto, 0, 0);
    NFSe.Status := StrToEnumerado(ok, Leitor.rCampo(tcStr, 'anulada'), ['N','S'], [srNormal, srCancelado]);
    NFSe.Cancelada := StrToSimNaoInFisc(ok, Leitor.rCampo(tcStr, 'cancelada')); {Jozimar}
    NFSe.MotivoCancelamento := Leitor.rCampo(tcStr, 'motCanc');                   {Jozimar}
    NFSe.InfID.ID := OnlyNumber(NFSe.CodigoVerificacao);
    NFSe.ChaveNFSe := Leitor.rCampo(tcStr, 'refNF');
    NFSe.Producao  := StrToSimNao(ok, Leitor.rCampo(tcStr, 'ambienteEmi'));
  end;

  if (Leitor.rExtrai(1, 'prest') <> '') then
  begin
    NFSe.Prestador.Cnpj                := Leitor.rCampo(tcStr, 'CNPJ');
    NFSe.Prestador.InscricaoEstadual   := Leitor.rCampo(tcStr, 'IE');
    NFSe.PrestadorServico.RazaoSocial  := Leitor.rCampo(tcStr, 'xNome');
    NFSe.PrestadorServico.NomeFantasia := Leitor.rCampo(tcStr, 'xFant');
    NFSe.Prestador.InscricaoMunicipal  := Leitor.rCampo(tcStr, 'IM');

    NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj := NFSe.Prestador.Cnpj;
    NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSe.Prestador.InscricaoMunicipal;
    NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoEstadual := NFSe.Prestador.InscricaoEstadual;

    NFSe.PrestadorServico.Contato.Telefone := Leitor.rCampo(tcStr, 'fone');
    NFSe.PrestadorServico.Contato.Email    := Leitor.rCampo(tcStr, 'xEmail');

    // 1=Simples, 2=SIMEI, 3=Normal
    if Leitor.rCampo(tcStr, 'regimeTrib') = '1' then
       NFSe.RegimeEspecialTributacao := retSimplesNacional
    else
    if Leitor.rCampo(tcStr, 'regimeTrib') = '2' then
       NFSe.RegimeEspecialTributacao := retMicroempresarioEmpresaPP
    else
    if Leitor.rCampo(tcStr, 'regimeTrib') = '3' then
       NFSe.RegimeEspecialTributacao := retLucroReal;

    if (Leitor.rExtrai(2, 'end') <> '') then
    begin
      NFSe.PrestadorServico.Endereco.Endereco        := Leitor.rCampo(tcStr, 'xLgr');
      NFSe.PrestadorServico.Endereco.Numero          := Leitor.rCampo(tcStr, 'nro');
      NFSe.PrestadorServico.Endereco.Complemento     := Leitor.rCampo(tcStr, 'xCpl');
      NFSe.PrestadorServico.Endereco.Bairro          := Leitor.rCampo(tcStr, 'xBairro');
      NFSe.PrestadorServico.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'cMun');

      NFSe.PrestadorServico.Endereco.xMunicipio := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));

      NFSe.PrestadorServico.Endereco.UF  := Leitor.rCampo(tcStr, 'UF');
      NFSe.PrestadorServico.Endereco.CEP := Leitor.rCampo(tcStr, 'CEP');
      NFSe.PrestadorServico.Endereco.CodigoPais := Leitor.rCampo(tcInt, 'cPais');
      NFSe.PrestadorServico.Endereco.xPais := Leitor.rCampo(tcStr, 'xPais');
    end;
  end;

  if (Leitor.rExtrai(1, 'TomS') <> '') then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := Leitor.rCampoCNPJCPF;
    NFSe.Tomador.RazaoSocial := Leitor.rCampo(tcStr, 'xNome');

    NFSe.Tomador.Contato.Email := Leitor.rCampo(tcStr, 'xEmail');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := Leitor.rCampo(tcStr, 'IE');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := Leitor.rCampo(tcStr, 'IM');
    NFSe.Tomador.Contato.Telefone := Leitor.rCampo(tcStr, 'fone');

    if (Leitor.rExtrai(2, 'ender') <> '') then
    begin
      NFSe.Tomador.Endereco.Endereco        := Leitor.rCampo(tcStr, 'xLgr');
      NFSe.Tomador.Endereco.Numero          := Leitor.rCampo(tcStr, 'nro');
      NFSe.Tomador.Endereco.Complemento     := Leitor.rCampo(tcStr, 'xCpl');
      NFSe.Tomador.Endereco.Bairro          := Leitor.rCampo(tcStr, 'xBairro');
      NFSe.Tomador.Endereco.CodigoMunicipio := Leitor.rCampo(tcStr, 'cMun');
      NFSe.Tomador.Endereco.xMunicipio      := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
      NFSe.Tomador.Endereco.UF              := Leitor.rCampo(tcStr, 'UF');
      NFSe.Tomador.Endereco.CEP             := Leitor.rCampo(tcStr, 'CEP');
      NFSe.Tomador.Endereco.CodigoPais      := Leitor.rCampo(tcInt, 'cPais');
      NFSe.Tomador.Endereco.xPais           := Leitor.rCampo(tcStr, 'xPais');
    end;
  end;

  Nfse.Servico.MunicipioIncidencia := 0;
  // Detalhes dos serviços
  Item := 0;
  while (Leitor.rExtrai(1, 'det', '', Item + 1) <> '') do
  begin

    if Leitor.rExtrai(2, 'serv') <> '' then
    begin
      if Nfse.Servico.MunicipioIncidencia = 0 then
         Nfse.Servico.MunicipioIncidencia := Leitor.rCampo(tcStr, 'localTributacao');
      Nfse.Servico.ItemServico.Add;
      Nfse.Servico.ItemServico[Item].codServ       := Leitor.rCampo(tcStr, 'cServ');
      Nfse.Servico.ItemServico[Item].CodLCServ     := Leitor.rCampo(tcStr, 'cLCServ');
      Nfse.Servico.ItemServico[Item].Descricao     := Leitor.rCampo(tcStr, 'xServ');
      Nfse.Servico.ItemServico[Item].Discriminacao := FNfse.Servico.ItemServico[Item].Codigo+' - '+FNfse.Servico.ItemServico[Item].Descricao;
      Nfse.Servico.ItemServico[Item].Quantidade    := Leitor.rCampo(tcDe4, 'qTrib');
      Nfse.Servico.ItemServico[Item].ValorUnitario := Leitor.rCampo(tcDe3, 'vUnit');
      Nfse.Servico.ItemServico[Item].ValorTotal    := Leitor.rCampo(tcDe3, 'vServ'); //Jozimar @@
      Nfse.Servico.ItemServico[Item].ValorServicos := Leitor.rCampo(tcDe2, 'vServ');
      Nfse.Servico.ItemServico[Item].DescontoIncondicionado := Leitor.rCampo(tcDe2, 'vDesc');
      // ISSQN
      Nfse.Servico.ItemServico[Item].BaseCalculo := Leitor.rCampo(tcDe2, 'vBCISS');
      Nfse.Servico.ItemServico[Item].Aliquota    := Leitor.rCampo(tcDe2, 'pISS');
      Nfse.Servico.ItemServico[Item].ValorIss    := Leitor.rCampo(tcDe2, 'vISS');
      // Retenções
      NFSe.Servico.ItemServico.Items[Item].ValorIr     := Leitor.rCampo(tcDe2, 'vRetIR');
      NFSe.Servico.ItemServico.Items[Item].ValorPis    := Leitor.rCampo(tcDe2, 'vRetPISPASEP');
      NFSe.Servico.ItemServico.Items[Item].ValorCofins := Leitor.rCampo(tcDe2, 'vRetCOFINS');
      NFSe.Servico.ItemServico.Items[Item].ValorCsll   := Leitor.rCampo(tcDe2, 'vRetCSLL');
      NFSe.Servico.ItemServico.Items[Item].ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
    end;
    if (Leitor.rExtrai(2, 'ISSST') <> '') then
    begin
      NFSe.Servico.ItemServico.Items[Item].AlicotaISSST := Leitor.rCampo(tcDe2, 'pISSST');
      NFSe.Servico.ItemServico.Items[Item].ValorISSST := Leitor.rCampo(tcDe2, 'vISSST');
    end;

    inc(Item);
  end;

  Item := 0;
  if (Leitor.rExtrai(1, 'despesas') <> '') then
  begin
    NFSe.Despesa.Add;
    NFSe.Despesa.Items[Item].nItemDesp := Leitor.rCampo(tcStr, 'nItemDesp');
    NFSe.Despesa.Items[Item].xDesp := Leitor.rCampo(tcStr, 'xDesp');
    NFSe.Despesa.Items[Item].dDesp := Leitor.rCampo(tcDat, 'dDesp');
    NFSe.Despesa.Items[Item].vDesp := Leitor.rCampo(tcDe2, 'vDesp');
    inc(Item);
  end;

  if (Leitor.rExtrai(1, 'total') <> '') then
  begin
    NFSe.Servico.Valores.ValorServicos          := Leitor.rCampo(tcDe2, 'vServ');
    NFSe.Servico.Valores.DescontoIncondicionado := Leitor.rCampo(tcDe2, 'vDesc');
    NFSe.Servico.Valores.ValorLiquidoNfse       := Leitor.rCampo(tcDe2, 'vtLiq');

    NFSe.Servico.Valores.ValorDespesasNaoTributaveis := Leitor.rCampo(tcDe2, 'vtDespesas');

    if (Leitor.rExtrai(2, 'ISS') <> '') then
    begin
      NFSe.Servico.Valores.BaseCalculo := Leitor.rCampo(tcDe2, 'vBCISS');
      NFSe.Servico.Valores.ValorIss    := Leitor.rCampo(tcDe2, 'vISS');
      NFSe.Servico.Valores.ValorIssRetido := Leitor.rCampo(tcDe2, 'vSTISS');

      if NFSe.Servico.Valores.ValorIssRetido > 0 then
      begin
        NFSe.Servico.Valores.IssRetido   := stRetencao;
        //Dados está sendo buscando na linha no inicio do metodo
        //NFSe.Servico.MunicipioIncidencia := StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0);
      end;
    end;

    if (Leitor.rExtrai(2, 'Ret') <> '') then
    begin
        // Retenções
      NFSe.Servico.Valores.ValorIr     := Leitor.rCampo(tcDe2, 'vRetIR');
      NFSe.Servico.Valores.ValorPis    := Leitor.rCampo(tcDe2, 'vRetPISPASEP');
      NFSe.Servico.Valores.ValorCofins := Leitor.rCampo(tcDe2, 'vRetCOFINS');
      NFSe.Servico.Valores.ValorCsll   := Leitor.rCampo(tcDe2, 'vRetCSLL');
      NFSe.Servico.Valores.ValorInss   := Leitor.rCampo(tcDe2, 'vRetINSS');
    end;
  end;

  if (Leitor.rExtrai(1, 'transportadora') <> '') then
  begin
    NFSe.Transportadora.xNomeTrans := Leitor.rCampo(tcStr, 'xNomeTrans');
    NFSe.Transportadora.xCpfCnpjTrans := Leitor.rCampo(tcStr, 'xCpfCnpjTrans');
    NFSe.Transportadora.xInscEstTrans := Leitor.rCampo(tcStr, 'xInscEstTrans');
    NFSe.Transportadora.xPlacaTrans := Leitor.rCampo(tcStr, 'xPlacaTrans');
    NFSe.Transportadora.xEndTrans := Leitor.rCampo(tcStr, 'xEndTrans');
    NFSe.Transportadora.cMunTrans := Leitor.rCampo(tcStr, 'cMunTrans');
    NFSe.Transportadora.xMunTrans := Leitor.rCampo(tcStr, 'xMunTrans');
    NFSe.Transportadora.xUFTrans := Leitor.rCampo(tcStr, 'xUfTrans');
    NFSe.Transportadora.cPaisTrans := Leitor.rCampo(tcStr, 'cPaisTrans');
    NFSe.Transportadora.vTipoFreteTrans := TnfseFrete(Leitor.rCampo(tcStr, 'vTipoFreteTrans'));
  end;

  if (Leitor.rExtrai(1, 'faturas') <> '') then
  begin
    Item := 0;
    while (Leitor.rExtrai(2, 'fat', '', Item + 1) <> '') do
    begin
      Nfse.CondicaoPagamento.Parcelas.Add;
      Nfse.CondicaoPagamento.Parcelas[Item].Parcela        := Leitor.rCampo(tcStr, 'nFat');
      Nfse.CondicaoPagamento.Parcelas[Item].DataVencimento := Leitor.rCampo(tcDat, 'dVenc');
      Nfse.CondicaoPagamento.Parcelas[Item].Valor          := Leitor.rCampo(tcDe2, 'vFat');

      inc(Item);
    end;
  end;

  Item := 0;
  NFSe.OutrasInformacoes := '';
  while (Leitor.rExtrai(1, 'infAdic', '', Item + 1) <> '') do
  begin
    NFSe.OutrasInformacoes := NFSe.OutrasInformacoes + Leitor.rCampo(tcStr, 'infAdic');
    inc(Item);
  end;
  Result := True;
end;

end.
