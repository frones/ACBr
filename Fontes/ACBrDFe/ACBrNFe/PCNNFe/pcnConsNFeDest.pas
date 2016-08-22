////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnConsNFeDest;

interface

uses
  SysUtils, Classes, pcnConversao, pcnConversaoNFe, pcnGerador ;

type

  TConsNFeDest = class(TPersistent)
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FCNPJ: String;
    FindNFe: TpcnIndicadorNFe;
    FindEmi: TpcnIndicadorEmissor;
    FultNSU: String;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    function ObterNomeArquivo: string;
  published
    property Gerador: TGerador            read FGerador write FGerador;
    property tpAmb: TpcnTipoAmbiente      read FtpAmb   write FtpAmb;
    property CNPJ: String                 read FCNPJ    write FCNPJ;
    property indNFe: TpcnIndicadorNFe     read FindNFe  write FindNFe;
    property indEmi: TpcnIndicadorEmissor read FindEmi  write FindEmi;
    property ultNSU: String               read FultNSU  write FultNSU;
    property Versao: String               read FVersao  write FVersao;
  end;

implementation

Uses pcnAuxiliar,
  ACBrUtil;

{ TConsNFeDest }

constructor TConsNFeDest.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TConsNFeDest.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsNFeDest.ObterNomeArquivo: string;
var
  DataHora: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
  AAAAMMDDTHHMMSS: string;
begin
  Datahora := now;
  DecodeTime(DataHora, Hour, Min, Sec, Milli);
  DecodeDate(DataHora, Year, Month, Day);
  AAAAMMDDTHHMMSS := IntToStrZero(Year, 4) + IntToStrZero(Month, 2) + IntToStrZero(Day, 2) +
    IntToStrZero(Hour, 2) + IntToStrZero(Min, 2) + IntToStrZero(Sec, 2);
  Result := AAAAMMDDTHHMMSS + '-con-nfe-dest.xml';
end;

function TConsNFeDest.GerarXML: Boolean;
var
 sDoc, sNSU: String;
begin
  Gerador.ArquivoFormatoXML := '';
  
  Gerador.wGrupo('consNFeDest ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'IP03', 'tpAmb', 001, 001, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'IP04', 'xServ', 018, 018, 1, 'CONSULTAR NFE DEST', DSC_XSERV);

  sDoc := OnlyNumber( FCNPJ );
  Gerador.wCampo(tcStr, 'IP05', 'CNPJ', 014, 014, 1, sDoc , DSC_CNPJ);
  if not ValidarCNPJ( sDoc ) then Gerador.wAlerta('IP05', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);

  Gerador.wCampo(tcStr, 'IP06', 'indNFe', 001, 001, 1, IndicadorNFeToStr(FindNFe), DSC_INDNFE);
  Gerador.wCampo(tcStr, 'IP07', 'indEmi', 001, 001, 1, IndicadorEmissorToStr(FindEmi), DSC_INDEMI);

  sNSU := FultNSU;
  if sNSU = '' then sNSU := '0';
  Gerador.wCampo(tcStr, 'IP08', 'ultNSU', 001, 015, 1, sNSU, DSC_ULTNSU);
  Gerador.wGrupo('/consNFeDest');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

