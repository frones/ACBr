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

{*******************************************************************************
|* Historico
|*
|* 13/03/2017: Andre Ferreira de Moraes
|*  - Primeira versão
*******************************************************************************}

{$I ACBr.inc}

unit pcnSATConsulta;

interface

uses
  SysUtils, Classes, pcnConversao, pcnGerador, pcnConsts;

type

  TSATConsulta = class(TObject)
  private
    FGerador        : TGerador;
    FtpAmb          : TpcnTipoAmbiente;
    FcUF            : Integer;
    FversaoDados    : String;
    FnserieSAT      : Integer;
    FdhInicial      : TDateTime;
    FdhFinal        : TDateTime;
    FchaveSeguranca : String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    function ObterNomeArquivo: string;
    property Gerador        : TGerador         read FGerador        write FGerador;
    property tpAmb          : TpcnTipoAmbiente read FtpAmb          write FtpAmb;
    property cUF            : Integer          read FcUF            write FcUF;
    property versaoDados    : String           read FversaoDados    write FversaoDados;
    property nserieSAT      : Integer          read FnserieSAT      write FnserieSAT;
    property dhInicial      : TDateTime        read FdhInicial      write FdhInicial;
    property dhFinal        : TDateTime        read FdhFinal        write FdhFinal;
    property chaveSeguranca : String           read FchaveSeguranca write FchaveSeguranca;
  end;

implementation

Uses pcnAuxiliar;

{ TSATConsulta }

constructor TSATConsulta.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
end;

destructor TSATConsulta.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TSATConsulta.ObterNomeArquivo: string;
var
  DataHora: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
  AAAAMMDDTHHMMSS: string;
begin
  Datahora:=now;
  DecodeTime(DataHora, Hour, Min, Sec, Milli);
  DecodeDate(DataHora, Year, Month, Day);
  AAAAMMDDTHHMMSS := IntToStrZero(Year, 4) + IntToStrZero(Month, 2) + IntToStrZero(Day, 2) +
    IntToStrZero(Hour, 2) + IntToStrZero(Min, 2) + IntToStrZero(Sec, 2);
  Result := AAAAMMDDTHHMMSS + '-ped-con-sat.xml';
end;

function TSATConsulta.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('consLote ' + NAME_SPACE_SAT + ' versao="' + versaoDados + '"');
  Gerador.wCampo(tcInt, 'SAT01', 'nserieSAT', 9, 9, 1, nserieSAT, DSC_NSERIESAT);
  Gerador.wCampo(tcStr, 'SAT02', 'dhInicial  ', 14, 14, 1, DateTimeToDataHora(dhInicial), DSC_DHINICIAL);
  Gerador.wCampo(tcStr, 'SAT03', 'dhFinal  ', 14, 14, 1, DateTimeToDataHora(dhFinal), DSC_DHFINAL);
  Gerador.wCampo(tcStr, 'SAT04', 'chaveSeguranca', 0, 999, 1, chaveSeguranca, DSC_CHAVESEGURANCA);
  Gerador.wGrupo('/consLote');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

