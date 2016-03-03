<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html" encoding="UTF-8"/>
	<xsl:template match="/">
		<xsl:apply-templates select="/MD_Metadata"/>
	</xsl:template>
	<xsl:template match="MD_Metadata">
		<html>
			<head>
				<TITLE>
					Metadato en formato ISO
				</TITLE>
				<STYLE>
						
						A:link {text-decoration: none;color: #087021;}
						A:visited {text-decoration: none;color: #087021}
						A:hover {text-decoration: none;color: #F1C630}
						
						body{text-align: center;}
						#contenedor {color: #333333; width:95%; margin:10px auto; font-family: Verdana, Arial; font-size: 12px;text-align: left;}
						fieldset {padding: 15px; margin-top: 15px;  }
						fieldset table {margin-top: 5px; width: 100%; border-style:solid;border-width: 0px; border-top-width: 1px; border-left-width: 1px;border-color:#808080; }
						legend {color: #087021;}
						h1{padding: 2px;text-align: center;color: #606060; font-size: 18px; border-style:dotted; border-width: 2px;border-color:#808080}
						h2{padding: 2px;text-align: center; background-color: #097021; color: white;font-size: 20px;}
						h3{padding: 2px; text-align: left; background-color: #f0be12; color: white; font-size: 14px}
						#resumen {font-family: Verdana, Arial;padding: 10px;text-align: justify; color: #333333; font-size: 12px; border-style:solid; border-width: 1px;border-color:#808080}
						#titulo_tabla {color: #333333; font-family: Verdana, Arial; font-size: 12px; padding: 2px; padding-left: 5px; padding-right: 5px; font-weight: bold; width: 25%;background-color: #e9f2e3;border-style:solid; border-width: 0px; border-bottom-width: 1px;border-right-width: 1px;border-color:#808080}
						#indice {margin-left: 31px; margin-right: 17px; margin-top: 12px; margin-bottom: 22px;}
						#contenido_tabla {color: #333333; font-family: Verdana, Arial; font-size: 12px; padding: 2px; padding-left: 5px; padding-right: 5px; background-color: #f0d878;border-style:solid; border-width: 1px;border-color:#808080;border-width: 0px; border-bottom-width: 1px;border-right-width: 1px; }
						#info {color: #555555; font-family: Verdana, Arial; font-size: 12px; font-weight: italic}
						#tabla_interna {/*margin: 2px;*/}
						#tabla_interna_titulo {font-family: Verdana, Arial; font-size: 12px; padding: 2px; padding-left: 5px; background-color: #a1a292; font-weight: bold;border-style:solid;border-color:#808080; border-width: 0px; border-bottom-width: 1px;border-right-width: 1px;}
						#tabla_interna_contenido {font-family: Verdana, Arial; font-size: 12px; padding: 2px; padding-left: 5px; background-color: #ececec; border-style:solid; border-color:#808080; border-width: 0px; border-bottom-width: 1px; border-right-width: 1px;}
						#enlace_central {text-align: center;margin-top: 5px;}
						
						
						#saltoDePagina
						{
							border-style:solid;
							PAGE-BREAK-AFTER: always
						}
				</STYLE>
			</head>
			<body>
				<div id="contenedor">
					<A name="inicio"/>
					<h1>RED DE INFORMACIÓN AMBIENTAL DE ANDALUCÍA</h1>
					<h2><xsl:value-of select="/MD_Metadata/identificationInfo/MD_DataIdentification/citation/CI_Citation/title"/></h2>
					<fieldset>
						<legend>Resumen</legend>
						<xsl:value-of select="identificationInfo/MD_DataIdentification/abstract"/>
					</fieldset>
					<h3>Índice</h3>
					<UL id="indice">
						<LI>
							<A HREF="#referenceSystemInfo">Información de los metadatos</A>
						</LI>
						<LI>
							<A HREF="#identificationInfo/MD_DataIdentification">Información de identificación</A>
						</LI>
						<LI>
							<A HREF="#dataQualityInfo/DQ_DataQuality">Datos de calidad</A>
						</LI>
						<LI>
							<A HREF="#distributionInfo/MD_Distribution">Información de distribución</A>
						</LI>
					</UL>
					<hr/>
					<xsl:apply-templates select="referenceSystemInfo"/>
					<xsl:apply-templates select="identificationInfo/MD_DataIdentification"/>
					<xsl:apply-templates select="dataQualityInfo/DQ_DataQuality"/>
					<xsl:apply-templates select="distributionInfo/MD_Distribution"/>
					<div id="enlace_central">
						<a href="#inicio">Inicio</a>
					</div>
				</div>
			</body>
		</html>
	</xsl:template>
	<!-- =============================================== -->
	<!-- ======  INFORMACIÓN DE LOS METADATOS  ========= -->
	<!-- =============================================== -->
	<xsl:template match="referenceSystemInfo">
		<A name="referenceSystemInfo"/>
		<fieldset>
			<legend>Información de los metadatos</legend>
			<!-- Información general -->
			<br/><span id="info">Información general &gt;&gt;</span>
			<table cellspacing="0">
				<tr><td id="titulo_tabla">Identificador del archivo de metadatos:</td>
					<td id="contenido_tabla">
						<xsl:value-of select="../fileIdentifier"/> </td></tr>
				<tr><td id="titulo_tabla">Idioma:</td>
					<td id="contenido_tabla">
						<xsl:value-of select="../language"/> </td></tr>
				<tr><td id="titulo_tabla">Conjunto de caracteres de los metadatos: </td>
					<td id="contenido_tabla">
						<!-- Codificacion de conjunto de caracteres -->
						<!-- ====================================== -->
						<xsl:choose>
							<xsl:when test="../characterSet = '001'">
					ucs2
				</xsl:when>
							<xsl:when test="../characterSet = '002'">
					ucs4
				</xsl:when>
							<xsl:when test="../characterSet = '003'">
					utf7
				</xsl:when>
							<xsl:when test="../characterSet = '004'">
					utf8
				</xsl:when>
							<xsl:when test="../characterSet = '005'">
					utf16
				</xsl:when>
							<xsl:when test="../characterSet = '006'">
					8859part1
				</xsl:when>
							<xsl:when test="../characterSet = '007'">
					8859part2
				</xsl:when>
							<xsl:when test="../characterSet = '008'">
					8859part3
				</xsl:when>
							<xsl:when test="../characterSet = '009'">
					8859part4
				</xsl:when>
							<xsl:when test="../characterSet = '010'">
					8859part5
				</xsl:when>
							<xsl:when test="../characterSet = '011'">
					8859part6
				</xsl:when>
							<xsl:when test="../characterSet = '012'">
					8859part7
				</xsl:when>
							<xsl:when test="../characterSet = '013'">
					8859part8
				</xsl:when>
							<xsl:when test="../characterSet = '014'">
					8859part9
				</xsl:when>
							<xsl:when test="../characterSet = '015'">
					8859part10
				</xsl:when>
							<xsl:when test="../characterSet = '016'">
					8859part11
				</xsl:when>
							<xsl:when test="../characterSet = '017'">
					Reserved for future use
				</xsl:when>
							<xsl:when test="../characterSet = '018'">
					8859part13
				</xsl:when>
							<xsl:when test="../characterSet = '019'">
					8859part14
				</xsl:when>
							<xsl:when test="../characterSet = '020'">
					8859part15
				</xsl:when>
							<xsl:when test="../characterSet = '021'">
					8859part16
				</xsl:when>
							<xsl:when test="../characterSet = '022'">
					jis
				</xsl:when>
							<xsl:when test="../characterSet = '023'">
					shiftJIS
				</xsl:when>
							<xsl:when test="../characterSet = '024'">
					eucJP
				</xsl:when>
							<xsl:when test="../characterSet = '025'">
					sAscii
				</xsl:when>
							<xsl:when test="../characterSet = '026'">
					ebcdic
				</xsl:when>
							<xsl:when test="../characterSet = '027'">
					eucKR
				</xsl:when>
							<xsl:when test="../characterSet = '028'">
					big5
				</xsl:when>
							<xsl:when test="../characterSet = '029'">
					GB2312
				</xsl:when>
							<xsl:otherwise>
					&#160;
				</xsl:otherwise>
						</xsl:choose>
					</td></tr>
				<tr><td id="titulo_tabla">Fecha de creación del metadato:</td>
					<td id="contenido_tabla">
						<xsl:value-of select="../dateStamp"/>
					</td></tr>
				<tr><td id="titulo_tabla">Norma de metadatos: </td>
					<td id="contenido_tabla">
						<xsl:value-of select="../metadataStandardName"/> </td></tr>
				<tr><td id="titulo_tabla">Versión de la norma de metadatos: </td>
					<td id="contenido_tabla">
						<xsl:value-of select="../metadataStandardVersion"/> </td></tr>
				<tr><td id="titulo_tabla">Nivel jerárquico: </td>
					<td id="contenido_tabla">
						<!-- Codificacion de Nivel Jerárquico -->
						<!-- ====================================== -->
						<xsl:choose>
							<xsl:when test="../hierarchyLevel= '001'">
					Atributo
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '002'">
					Tipo de Atributo
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '003'">
					Colección de Hardware
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '004'">
					Colección de Sesión
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '005'">
					Conjunto de Datos
				</xsl:when>
							<xsl:when test="../hierarchyLevel= '006'">
					Series
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '007'">
					Conjuto de Datos no Geográficos
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '008'">
					Grupo de Dimensiones
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '009'">
					Elemento
				</xsl:when>
							<xsl:when test="../hierarchyLevel= '010'">
					Tipo de Elemento
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '011'">
					Tipo de Propiedad
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '012'">
					Conjunto de Sesión
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '013'">
					Software
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '014'">
					Servicio
				</xsl:when>
							<xsl:when test="../hierarchyLevel = '015'">
					Modelo
				</xsl:when>
							<xsl:otherwise>
					&#160;
							</xsl:otherwise>
						</xsl:choose>
					</td></tr>
				<tr><td id="titulo_tabla">Sistema de referencia: </td>
					<td id="contenido_tabla">
						<xsl:choose>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:3034'">
							ETRS89/ETRS-LCC Para la cartografía pan-Europeo a escalas menores o iguales a 1:500.000 IDEE
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:3035'">
							ETRS89/ETRS-LAEA Para representación y análisis estadístico pan-Europeos IDEE
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:4230'">
							Coordenadas Geográficas ED50
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:4258'">
							Coordenadas Elipsoidales ETRS89 IDEE
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:4267'">
							Coordenadas Geográficas North American Datum 1927 (NAD 27)
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:4269'">
 							Coordenadas Geográficas North American Datum 1983 (NAD 83)
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:4324'">
							Coordenadas Geográficas WGS 72BE Transit Broadcast Ephemeris
                        </xsl:when>
						    <xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:4326'">
							Coordenadas Geográficas WGS84
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:23028'">
							Proyección UTM ED50 Huso 28 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:23029'">
							Proyección UTM ED50 Huso 29 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:23030'">
							Proyección UTM ED50 Huso 30 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:23031'">
							Proyección UTM ED50 Huso 31 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:25828'">
							Proyección UTM ETRS89 Huso 28 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:25829'">
							Proyección UTM ETRS89 Huso 29 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:25830'">
							Proyección UTM ETRS89 Huso 30 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:25831'">
							Proyección UTM ETRS89 Huso 31 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:32628'">
							Proyección UTM WGS84 Huso 28 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:32629'">
							Proyección UTM WGS84 Huso 29 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:32630'">
							Proyección UTM WGS84 Huso 30 N
						</xsl:when>
							<xsl:when test="MD_ReferenceSystem/referenceSystemIdentifier = 'EPSG:32631'">
							Proyección UTM WGS84 Huso 31 N
						</xsl:when>
					</xsl:choose>
				 </td></tr>
			</table>
			<A name="contact/CI_ResponsibleParty"/>
			<!-- Información de contacto -->
			<br/><span id="info">Contacto responsable de los metadatos&gt;&gt;</span>
			<table cellspacing="0">
				<tr>
					<td id="titulo_tabla">Organización:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/organisationName"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Puesto:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/positionName"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Rol:</td>
					<td id="contenido_tabla">
						<!-- Codificacion de Rol: papel que desempeña 
					 la persona que crea los metadatos -->
						<!-- ====================================== -->
						<xsl:choose>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '001'">
						Proveedor del Recurso
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '002'">
						Conservador
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '003'">
						Propietario
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '004'">
						Usuario
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '005'">
						Distribuidor
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '006'">
						Creador
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '007'">
						Punto de Contacto
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '008'">
						Investigador Principal
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '009'">
						Procesador
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '010'">
						Editor
					</xsl:when>
							<xsl:when test="../contact/CI_ResponsibleParty/role = '011'">
						Autor
					</xsl:when>
							<xsl:otherwise>
						&#160;
					</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Dirección:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/address/CI_Address/deliveryPoint"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">C.P. :</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/address/CI_Address/postalCode"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Teléfonos:</td>
					<td id="contenido_tabla">
						<xsl:for-each select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/phone/CI_Telephone/voice">
							<xsl:value-of select="."/><br/>
						</xsl:for-each>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Fax:</td>
					<td id="contenido_tabla">
						<xsl:for-each select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/phone/CI_Telephone/facsimile">
							<xsl:value-of select="."/><br/>
						</xsl:for-each>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Área administrativa:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/address/CI_Address/administrativeArea"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Ciudad:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/address/CI_Address/city"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">País:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/address/CI_Address/country"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Correo electrónico:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/address/CI_Address/electronicMailAddress"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Recurso en línea:</td>
					<td id="contenido_tabla"><xsl:value-of select="../contact/CI_ResponsibleParty/contactInfo/CI_Contact/onlineResource/CI_OnlineResource/linkage"/> </td>
				</tr>
			</table>
			<!-- Descomentar para salto de impresión -->
			<!-- <h1 id="saltoDepagina" style="border-style: none;"></h1> -->
		</fieldset>
	</xsl:template>
	<!-- =============================================== -->
	<!-- ========  FIN INFORMACIÓN DE LOS METADATOS  ======== -->
	<!-- =============================================== -->
	<!-- =============================================== -->
	<!-- ===============  DATOS DE IDENTIFICACIÓN  ============ -->
	<!-- =============================================== -->
	<xsl:template match="identificationInfo/MD_DataIdentification">
		<A name="identificationInfo/MD_DataIdentification"/>
		<fieldset>
			<legend>Información de identificación</legend>
			<!--  Información de los datos -->
			<br/><span id="info">Información de los datos &gt;&gt;</span>
			<table cellspacing="0">
				<tr>
					<td id="titulo_tabla">Título:</td>
					<td id="contenido_tabla"><xsl:value-of select="citation/CI_Citation/title"/>&#160;</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Resumen:</td>
					<td id="contenido_tabla"><xsl:value-of select="abstract"/>&#160;</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Propósito:</td>
					<td id="contenido_tabla"><xsl:value-of select="purpose"/>&#160;</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Agradecimientos:</td>
					<td id="contenido_tabla"><xsl:value-of select="credit"/>&#160;</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Forma de presentación:</td>
					<td id="contenido_tabla">
						<!-- Codificacion de Forma de Presentación  -->
						<!-- ====================================== -->
						<xsl:choose>
							<xsl:when test="citation/CI_Citation/presentationForm = '001'">
						Documento digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '002'">
						Copia Impresa del documento
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '003'">
						Imagen Digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '004'">
						Copia Impresa de la Imagen
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '005'">
						Mapa digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '006'">
						Copia impresa en papel
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '007'">
						Modelo digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '008'">
						Copia impresa del modelo
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '009'">
						Perfil digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '010'">
						Copia impresa digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '011'">
						Tabla digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '012'">
						Copia impresa de la tabla
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '013'">
						Video digital
					</xsl:when>
							<xsl:when test="citation/CI_Citation/presentationForm = '014'">
						Video en cinta
					</xsl:when>
							<xsl:otherwise>
						&#160;
					</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Tipo de representación espacial:</td>
					<td id="contenido_tabla">
						<xsl:choose>
							<xsl:when test="spatialRepresentationType = '001'">
							Vector
						</xsl:when>
							<xsl:when test="spatialRepresentationType = '002'">
							Grid
						</xsl:when>
							<xsl:when test="spatialRepresentationType = '003'">
							Tabla de texto
						</xsl:when>
							<xsl:when test="spatialRepresentationType = '004'">
							Tin
						</xsl:when>
							<xsl:when test="spatialRepresentationType = '005'">
							Modelo Stereo
						</xsl:when>
							<xsl:when test="spatialRepresentationType = '006'">
							Video
						</xsl:when>
							<xsl:otherwise>
							&#160;
						</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Fecha:</td>
					<td id="">
						<table cellspacing="0" id="tabla_interna">
							<tr>
								<td id="tabla_interna_titulo" align="center" width="25%">Tipo Fecha</td>
								<td id="tabla_interna_titulo" align="center" width="25%">Fecha</td>
							</tr>
							<!--  Fecha de Creacion -->
							<xsl:if test="citation/CI_Citation/dateCreacion != ''">
								<tr>
									<td id="tabla_interna_contenido" align="center">
										<xsl:choose>
											<xsl:when test="citation/CI_Citation/dateCreacion/CI_Date/dateType = '001'">
											Fecha de creación de los datos<br/>
											</xsl:when>
											<xsl:when test="citation/CI_Citation/dateCreacion/CI_Date/dateType = '002'">
											Fecha de publicación<br/>
											</xsl:when>
											<xsl:when test="citation/CI_Citation/dateCreacion/CI_Date/dateType = '003'">
											Fecha de revisión<br/>
											</xsl:when>
											<xsl:otherwise>
											&#160;<br/>
											</xsl:otherwise>
										</xsl:choose>
									</td>
									<td id="tabla_interna_contenido" align="center">
										<xsl:value-of select="citation/CI_Citation/dateCreacion/CI_Date/dateValue"/> 
								</td>
								</tr>
							</xsl:if>
							<!--  Fecha de Publicación -->
							<xsl:if test="citation/CI_Citation/datePublicacion != ''">
								<tr>
									<td id="tabla_interna_contenido" align="center">
										<xsl:choose>
											<xsl:when test="citation/CI_Citation/datePublicacion/CI_Date/dateType = '001'">
												Fecha de creación de los datos<br/>
											</xsl:when>
											<xsl:when test="citation/CI_Citation/datePublicacion/CI_Date/dateType = '002'">
												Fecha de publicación<br/>
											</xsl:when>
											<xsl:when test="citation/CI_Citation/datePublicacion/CI_Date/dateType = '003'">
												Fecha de revisión<br/>
											</xsl:when>
											<xsl:otherwise>
												&#160;<br/>
											</xsl:otherwise>
										</xsl:choose>
									</td>
									<td id="tabla_interna_contenido" align="center">
										<xsl:value-of select="citation/CI_Citation/datePublicacion/CI_Date/dateValue"/> 
									</td>
								</tr>
							</xsl:if>
							<!--  Fecha de Revision -->
							<xsl:if test="citation/CI_Citation/dateRevision!= ''">
								<tr>
									<td id="tabla_interna_contenido" align="center">
										<xsl:choose>
											<xsl:when test="citation/CI_Citation/dateRevision/CI_Date/dateType = '001'">
												Fecha de creación de los datos<br/>
											</xsl:when>
											<xsl:when test="citation/CI_Citation/dateRevision/CI_Date/dateType = '002'">
												Fecha de publicación<br/>
											</xsl:when>
											<xsl:when test="citation/CI_Citation/dateRevision/CI_Date/dateType = '003'">
												Fecha de revisión<br/>
											</xsl:when>
											<xsl:otherwise>
												&#160;<br/>
											</xsl:otherwise>
										</xsl:choose>
									</td>
									<td id="tabla_interna_contenido" align="center">
										<xsl:value-of select="citation/CI_Citation/dateRevision/CI_Date/dateValue"/> 
									</td>
								</tr>
							</xsl:if>
						</table>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Estado:</td>
					<td id="contenido_tabla"><xsl:value-of select="status"/> </td>
				</tr>
				<tr>
					<td id="titulo_tabla">Categorías:</td>
					<td id="contenido_tabla">
						<xsl:for-each select="category/topicCategory">
							<xsl:choose>
								<xsl:when test=". = '001'">
							Agricultura<br/>
								</xsl:when>
								<xsl:when test=". = '002'">
							Biota<br/>
								</xsl:when>
								<xsl:when test=". = '003'">
							Límites<br/>
								</xsl:when>
								<xsl:when test=". = '004'">
							Atmósfera climatología y Meteorología<br/>
								</xsl:when>
								<xsl:when test=". = '005'">
							Economía<br/>
								</xsl:when>
								<xsl:when test=". = '006'">
							Elevación<br/>
								</xsl:when>
								<xsl:when test=". = '007'">
							Medio Ambiente<br/>
								</xsl:when>
								<xsl:when test=". = '008'">
							Información Geocientífica<br/>
								</xsl:when>
								<xsl:when test=". = '009'">
							Salud<br/>
								</xsl:when>
								<xsl:when test=". = '010'">
							Cobertura de la Tierra con Mapas Básicos e Imágenes<br/>
								</xsl:when>
								<xsl:when test=". = '011'">
							Inteligencia Militar<br/>
								</xsl:when>
								<xsl:when test=". = '012'">
							Aguas Interiores<br/>
								</xsl:when>
								<xsl:when test=". = '013'">
							Localización<br/>
								</xsl:when>
								<xsl:when test=". = '014'">
							Océanos<br/>
								</xsl:when>
								<xsl:when test=". = '015'">
							Planeamiento Catastral<br/>
								</xsl:when>
								<xsl:when test=". = '016'">
							Sociedad<br/>
								</xsl:when>
								<xsl:when test=". = '017'">
							Estructuras<br/>
								</xsl:when>
								<xsl:when test=". = '018'">
							Transporte<br/>
								</xsl:when>
								<xsl:when test=". = '019'">
							Redes de suministro<br/>
								</xsl:when>
								<xsl:otherwise>
							&#160;<br/>
								</xsl:otherwise>
							</xsl:choose>
						</xsl:for-each>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Idioma:</td>
					<td id="contenido_tabla"><xsl:value-of select="language"/> </td>
				</tr>
				<tr><td id="titulo_tabla">Conjunto de caracteres de los datos: </td>
					<td id="contenido_tabla">
						<!-- Codificacion de conjunto de caracteres -->
						<!-- ====================================== -->
						<xsl:choose>
							<xsl:when test="characterSet = '001'">
					ucs2
				</xsl:when>
							<xsl:when test="characterSet = '002'">
					ucs4
				</xsl:when>
							<xsl:when test="characterSet = '003'">
					utf7
				</xsl:when>
							<xsl:when test="characterSet = '004'">
					utf8
				</xsl:when>
							<xsl:when test="characterSet = '005'">
					utf16
				</xsl:when>
							<xsl:when test="characterSet = '006'">
					8859part1
				</xsl:when>
							<xsl:when test="characterSet = '007'">
					8859part2
				</xsl:when>
							<xsl:when test="characterSet = '008'">
					8859part3
				</xsl:when>
							<xsl:when test="characterSet = '009'">
					8859part4
				</xsl:when>
							<xsl:when test="characterSet = '010'">
					8859part5
				</xsl:when>
							<xsl:when test="characterSet = '011'">
					8859part6
				</xsl:when>
							<xsl:when test="characterSet = '012'">
					8859part7
				</xsl:when>
							<xsl:when test="characterSet = '013'">
					8859part8
				</xsl:when>
							<xsl:when test="characterSet = '014'">
					8859part9
				</xsl:when>
							<xsl:when test="characterSet = '015'">
					8859part10
				</xsl:when>
							<xsl:when test="characterSet = '016'">
					8859part11
				</xsl:when>
							<xsl:when test="characterSet = '017'">
					Reserved for future use
				</xsl:when>
							<xsl:when test="characterSet = '018'">
					8859part13
				</xsl:when>
							<xsl:when test="characterSet = '019'">
					8859part14
				</xsl:when>
							<xsl:when test="characterSet = '020'">
					8859part15
				</xsl:when>
							<xsl:when test="characterSet = '021'">
					8859part16
				</xsl:when>
							<xsl:when test="characterSet = '022'">
					jis
				</xsl:when>
							<xsl:when test="characterSet = '023'">
					shiftJIS
				</xsl:when>
							<xsl:when test="characterSet = '024'">
					eucJP
				</xsl:when>
							<xsl:when test="characterSet = '025'">
					sAscii
				</xsl:when>
							<xsl:when test="characterSet = '026'">
					ebcdic
				</xsl:when>
							<xsl:when test="characterSet = '027'">
					eucKR
				</xsl:when>
							<xsl:when test="characterSet = '028'">
					big5
				</xsl:when>
							<xsl:when test="characterSet = '029'">
					GB2312
				</xsl:when>
							<xsl:otherwise>
					&#160;
				</xsl:otherwise>
						</xsl:choose>
					</td></tr>
			</table>
			<br/>
			<!-- Palabras clave -->
			<br/><span id="info">Palabras clave &gt;&gt;</span>
			<table cellspacing="0">
				<tr>
					<td id="titulo_tabla" style="width: 35%" align="center">Palabra</td><td id="titulo_tabla" style="width: 35%" align="center">Tipo</td><td id="titulo_tabla" style="width: 30%" align="center">Nombre tesauro</td>
				</tr>
				<xsl:for-each select="descriptiveKeywords/MD_Keywords">
					<tr>
						<td id="contenido_tabla">
							<xsl:for-each select="./keyword">
								<xsl:if test="position()!=1">
								 ,   
								</xsl:if>
								&#160;
								<xsl:if test="position() != last()">
									<xsl:value-of select="."/>
								</xsl:if>
								<xsl:if test="position() = last()">
									<xsl:value-of select="."/>
								</xsl:if>
							</xsl:for-each>
							&#160;
					</td>
						<td id="contenido_tabla" align="center">
							<!-- Codificacion de Rol: papel que desempeña 
						 la persona que crea los metadatos -->
							<!-- ====================================== -->
							<xsl:choose>
								<xsl:when test="./type = '001'">
								Disciplina
							</xsl:when>
								<xsl:when test="./type = '002'">
								Lugar
							</xsl:when>
								<xsl:when test="./type = '003'">
								Estrato
							</xsl:when>
								<xsl:when test="./type = '004'">
								Temporal
							</xsl:when>
								<xsl:when test="./type = '005'">
								Temático
							</xsl:when>
								<xsl:otherwise>
								&#160;
							</xsl:otherwise>
							</xsl:choose>
						</td>
						<td id="contenido_tabla" align="center"><xsl:value-of select="./thesaurusName"/> </td>
					</tr>
				</xsl:for-each>
			</table>
			<!-- Información geográfica -->
			<a name="sistreferencia"/>
			<br/><span id="info">Localización geográfica &gt;&gt; </span>
			<xsl:for-each select="extent/geographicElement">
				<table cellspacing="0">
					<tr>
						<td id="titulo_tabla">Coordenada Este:</td>
						<td id="contenido_tabla"><xsl:value-of select="EX_GeographicBoundingBox/eastBoundLongitude"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Coordenada Norte:</td>
						<td id="contenido_tabla"><xsl:value-of select="EX_GeographicBoundingBox/northBoundLatitude"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Coordenada Oeste:</td>
						<td id="contenido_tabla"><xsl:value-of select="EX_GeographicBoundingBox/westBoundLongitude"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Coordenada Sur:</td>
						<td id="contenido_tabla"><xsl:value-of select="EX_GeographicBoundingBox/southBoundLatitude"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Descripción de la extensión:</td>
						<td id="contenido_tabla"><xsl:value-of select="EX_GeographicDescription/geographicIdentifier/MD_Identifier/code"/> </td>
					</tr>
				</table><br/>
			</xsl:for-each>
			<!-- Descomentar para salto de impresión -->
			<!-- <h1 id="saltoDepagina" style="border-style: none;"></h1> -->
			<!-- Resolucion espacial: solo representamos este apartado para las representaciones espaciales Vector,  
	 Grid y Tin -->
			<xsl:choose>
				<xsl:when test="spatialRepresentationType = '001'"><!--  Vector -->
					<br/><span id="info">Resolución espacial &gt;&gt; </span>
					<table cellspacing="0">
						<tr>
							<td id="titulo_tabla">Denominador:</td>
							<td id="contenido_tabla"><xsl:value-of select="spatialResolution/MD_Resolution/equivalentScale/MD_RepresentativeFraction/denominator"/> </td>
						</tr>
					</table>
				</xsl:when>
				<xsl:when test="spatialRepresentationType = '002'"><!--  Grid -->
					<br/><span id="info">Resolución espacial &gt;&gt; </span>
					<table cellspacing="0">
						<tr>
							<td id="titulo_tabla">Distancia:</td>
							<td id="contenido_tabla"><xsl:value-of select="spatialResolution/MD_Resolution/distance/Distance/value"/> </td>
						</tr>
						<tr>
							<td id="titulo_tabla">Unidad de medida:</td>
							<td id="contenido_tabla"><xsl:value-of select="spatialResolution/MD_Resolution/distance/Distance/uom/uomLength/uomName"/> </td>
						</tr>
					</table>
				</xsl:when>
				<xsl:when test="spatialRepresentationType = '003'"><!--  Tabla de texto -->
				</xsl:when>
				<xsl:when test="spatialRepresentationType = '004'"><!--  Tin -->
					<br/><span id="info">Resolución espacial &gt;&gt; </span>
					<table cellspacing="0">
						<tr>
							<td id="titulo_tabla">Distancia:</td>
							<td id="contenido_tabla"><xsl:value-of select="spatialResolution/MD_Resolution/distance/Distance/value"/> </td>
						</tr>
						<tr>
							<td id="titulo_tabla">Unidad de medida:</td>
							<td id="contenido_tabla"><xsl:value-of select="spatialResolution/MD_Resolution/distance/Distance/uom/uomLength/uomName"/> </td>
						</tr>
					</table>
				</xsl:when>
				<xsl:when test="spatialRepresentationType = '005'"><!--  Modelo Stereo -->
				</xsl:when>
				<xsl:when test="spatialRepresentationType = '006'"><!--  Video -->
				</xsl:when>
				<xsl:otherwise>
				</xsl:otherwise>
			</xsl:choose>
			<!-- Restricciones legales -->
			<br/><span id="info">Restricciones legales &gt;&gt;</span>
			<table cellspacing="0">
				<tr>
					<td id="titulo_tabla">Restricción de acceso:</td>
					<td id="contenido_tabla">
						<!-- Codificacion Restricciones -->
						<!-- ========================== -->
						<xsl:choose>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '001'">
									Copyright
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '002'">
									Patente
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstrains = '003'">
									Pendiente de patentar
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '004'">
									Marca registrada
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '005'">
									Licencia
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '006'">
									Derecho de Propiedad Intelectual
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '007'">
									Restringido
								</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/accessConstraints = '008'">
									Otras Restricciones
								</xsl:when>
							<xsl:otherwise>
									&#160;
							</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Restricción de uso:</td>
					<td id="contenido_tabla">
						<!-- Codificacion Restricciones -->
						<!-- ========================== -->
						<xsl:choose>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '001'">
											Copyright
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '002'">
											Patente
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '003'">
											Pendiente de patentar
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '004'">
											Marca registrada
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '005'">
											Licencia
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '006'">
											Derecho de Propiedad Intelectual
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '007'">
											Restringido
										</xsl:when>
							<xsl:when test="resourceConstraints/MD_LegalConstraints/useConstraints = '008'">
											Otras Restricciones
										</xsl:when>
							<xsl:otherwise>
											&#160;
										</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla">Otras restricciones:</td>
					<td id="contenido_tabla"><xsl:value-of select="resourceConstraints/MD_LegalConstraints/otherConstraints"/> </td>
				</tr>
			</table>
			<!-- Información del contacto -->
			<xsl:for-each select="pointOfContact/CI_ResponsibleParty">
				<br/><span id="info">Contacto responsable del conjunto de datos (<xsl:value-of select="position()"/>) &gt;&gt;</span>
				<table cellspacing="0">
					<tr>
						<td id="titulo_tabla">Nombre de la organización:</td>
						<td id="contenido_tabla"><xsl:value-of select="./organisationName"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Puesto:</td>
						<td id="contenido_tabla"><xsl:value-of select="./positionName"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Rol:</td>
						<td id="contenido_tabla">
							<!-- Codificacion de Rol: papel que desempeña 
					 la persona que crea los metadatos -->
							<!-- ====================================== -->
							<xsl:choose>
								<xsl:when test="./role = '001'">
							Abastecedor del Recurso
						</xsl:when>
								<xsl:when test="./role = '002'">
							Guardián
						</xsl:when>
								<xsl:when test="./role = '003'">
							Propietario
						</xsl:when>
								<xsl:when test="./role = '004'">
							Usuario
						</xsl:when>
								<xsl:when test="./role = '005'">
							Distribuidor
						</xsl:when>
								<xsl:when test="./role = '006'">
							Creador
						</xsl:when>
								<xsl:when test="./role = '007'">
							Punto de Contacto
						</xsl:when>
								<xsl:when test="./role = '008'">
							Investigador Principal
						</xsl:when>
								<xsl:when test="./role = '009'">
							Procesador
						</xsl:when>
								<xsl:when test="./role = '010'">
							Publicador
						</xsl:when>
								<xsl:when test="./role = '011'">
							Autor
						</xsl:when>
						<xsl:otherwise>
							&#160;
						</xsl:otherwise>
							</xsl:choose>
						</td>
					</tr>
					<tr>
						<td id="titulo_tabla">Dirección:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/address/CI_Address/deliveryPoint"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Código postal:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/address/CI_Address/postalCode"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Teléfonos:</td>
						<td id="contenido_tabla">
							<xsl:for-each select="./contactInfo/CI_Contact/phone/CI_Telephone/voice">
								<xsl:value-of select="."/><br/>
							</xsl:for-each>
						</td>
					</tr>
					<tr>
						<td id="titulo_tabla">Fax:</td>
						<td id="contenido_tabla">
							<xsl:for-each select="./contactInfo/CI_Contact/phone/CI_Telephone/facsimile">
								<xsl:value-of select="."/><br/>
							</xsl:for-each>
						</td>
					</tr>
					<tr>
						<td id="titulo_tabla">Área administrativa:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/address/CI_Address/administrativeArea"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Ciudad:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/address/CI_Address/city"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">País:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/address/CI_Address/country"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Correo electrónico:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/address/CI_Address/electronicMailAddress"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla">Recurso en línea:</td>
						<td id="contenido_tabla"><xsl:value-of select="./contactInfo/CI_Contact/onlineResource/CI_OnlineResource/linkage"/> </td>
					</tr>
				</table>
				<br/>
			</xsl:for-each>
			<!-- Descomentar para salto de impresión -->
			<!-- <h1 id="saltoDepagina" style="border-style: none;"></h1> -->
		</fieldset>
	</xsl:template>
	<!-- =============================================== -->
	<!-- ===============  FIN DATOS DE IDENTIFICACIÓN  ============ -->
	<!-- =============================================== -->
	<!-- =============================================== -->
	<!-- ===============  DATOS DE CALIDAD  ============ -->
	<!-- =============================================== -->
	<xsl:template match="dataQualityInfo/DQ_DataQuality">
		<A name="dataQualityInfo/DQ_DataQuality"/>
		<fieldset>
			<legend>Datos de calidad</legend>
			<!--  Ámbito -->
			<br/><span id="info">Ámbito &gt;&gt;</span>
			<table cellspacing="0">
				<tr>
					<td id="titulo_tabla" style="width: 20%">Nivel jerárquico:</td>
					<td id="contenido_tabla" style="width: 80%">
						<!-- Codificacion de Nivel Jerárquico: 		-->
						<!-- ====================================== -->
						<xsl:choose>
							<xsl:when test="scope/DQ_Scope/level = '001'">
						Atributo
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '002'">
						Tipo de Atributo
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '003'">
						Hardware de captura
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '004'">
						Sesión de captura
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '005'">
						Conjunto de datos
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '006'">
						Series
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '007'">
						Conjunto de Datos no Geográficos
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '008'">
						Grupo de dimensiones
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '009'">
						Fenómeno
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '010'">
						Tipo de Fenómeno
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '011'">
						Tipo de Propiedad
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '012'">
						Sesión de campo
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '013'">
						Software
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '014'">
						Servicio
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '015'">
						Modelo
					</xsl:when>
							<xsl:when test="scope/DQ_Scope/level = '016'">
						División
					</xsl:when>
							<xsl:otherwise>
						&#160;
					</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<tr>
					<td id="titulo_tabla" colspan="2">Extensión del ámbito:<br/><br/>
						<table cellspacing="0" id="tabla_interna">
						<tr>
								<td id="tabla_interna_titulo" style="width: 17%">Coordenada Este:</td>
								<td id="tabla_interna_contenido" ><xsl:value-of select="scope/DQ_Scope/extent/EX_GeographicBoundingBox/eastBoundLongitude"/>&#160;</td>
							</tr>
							<tr>
								<td id="tabla_interna_titulo">Coordenada Norte:</td>
								<td id="tabla_interna_contenido" ><xsl:value-of select="scope/DQ_Scope/extent/EX_GeographicBoundingBox/northBoundLatitude"/>&#160;</td>
							</tr>
							<tr>
								<td id="tabla_interna_titulo">Coordenada Oeste:</td>
								<td id="tabla_interna_contenido" ><xsl:value-of select="scope/DQ_Scope/extent/EX_GeographicBoundingBox/westBoundLongitude"/>&#160;</td>
							</tr>	
							<tr>
								<td id="tabla_interna_titulo">Coordenada Sur:</td>
								<td id="tabla_interna_contenido" ><xsl:value-of select="scope/DQ_Scope/extent/EX_GeographicBoundingBox/southBoundLatitude"/>&#160;</td>
							</tr>
							<tr>
								<td id="tabla_interna_titulo" style="width: 20%">Descripción de la extensión:</td>
								<td id="tabla_interna_contenido" style="width: 80%"><xsl:value-of select="scope/DQ_Scope/extent/EX_Extent/description"/> </td>
							</tr>
						</table>
						<br/>
					</td>
				</tr>
			</table>
			<!--  Fin Ámbito -->
			<!--  Linajes -->
			<xsl:for-each select="lineage/LI_Lineage">
				<br/><span id="info">Información cualitativa (Linaje) &gt;&gt;</span>
				<table cellspacing="0">
					<tr>
						<td id="titulo_tabla" style="width: 20%">Descripción general del proceso:</td>
						<td id="contenido_tabla" style="width: 80%"><xsl:value-of select="statement"/> </td>
					</tr>
					<tr>
						<td id="titulo_tabla" colspan="2">Descripción:<br/><br/>
							<!-- Almacenamos las fechas en una variable 
						  <xsl:variable name="fechas">
							<xsl:for-each select="processStep/LI_ProcessStep/dateTime" >
								<xsl:sequence select="@value"/>
							</xsl:for-each>
						  </xsl:variable>
					-->
							<xsl:for-each select="processStep/LI_ProcessStep/description">
								<xsl:variable name="indiceActual" as="xs:integer">
									<xsl:value-of select="position()"/>
								</xsl:variable>
								<table cellspacing="0" id="tabla_interna">
									<tr>
										<td id="tabla_interna_titulo" width="17%">Paso:</td>
										<td id="tabla_interna_contenido">
											<xsl:value-of select="."/>
								 </td>
									</tr>
									<xsl:for-each select="../dateTime">
										<xsl:if test="position() = $indiceActual">
											<tr>
												<td id="tabla_interna_titulo">Fecha del evento:</td>
												<td id="tabla_interna_contenido">
													<xsl:value-of select="."/>
											 </td>
											</tr>
										</xsl:if>
									</xsl:for-each>
								</table>
								<br/>
							</xsl:for-each>
						</td>
					</tr>
					<tr>
						<td id="titulo_tabla" style="width: 20%">Fuentes:</td>
						<td style="width: 80%">
							<table cellspacing="0" id="tabla_interna">
								<tr>
									<td id="tabla_interna_titulo" align="center">Descripción</td>
								</tr>
								<xsl:for-each select="source/LI_Source">
									<tr>
										<td id="tabla_interna_contenido" align="center"><xsl:value-of select="description"/> </td>
									</tr>
								</xsl:for-each>
							</table>
						</td>
					</tr>
				</table>
			</xsl:for-each>
			<!--  Fin Linajes -->
			<!--  Información cuantitativa -->
			<br/><span id="info">Información cuantitativa &gt;&gt; </span>
			<table cellspacing="0">
				<tr>
					<td id="titulo_tabla" align="center" style="width: 15%">Tipo de informe</td>
					<td id="titulo_tabla" align="center" style="width: 25%">Descripción del método de evaluación</td>
					<td id="titulo_tabla" align="center" style="width: 15%">Método de evaluación</td>
					<td id="titulo_tabla" align="center" style="width: 25%">Descripción de la medida de los datos</td>
					<td id="titulo_tabla" align="center" style="width: 10%">Nombre de la medida</td>
					<td id="titulo_tabla" align="center" style="width: 10%">Resultado</td>
				</tr>
				<xsl:for-each select="report/*">
					<tr>
						<td id="contenido_tabla" align="center">
						<xsl:choose>
							<xsl:when test="type='DQ_QuantitativeAttributeAccuracy'">
					        Exactitud de los atributos cuantitativos
				            </xsl:when>
							
							<xsl:when test="type='DQ_NonQuantitativeAttributeAccuracy'">
					        Exactitud de los atributos no cuantitativos
				            </xsl:when>
							
							<xsl:when test="type='DQ_ThematicClassificationCorrectness'">
					        Corrección de la clasificación temática
				            </xsl:when>
							
							<xsl:when test="type='DQ_CompletenessComission'">
					        Compleción por comisión
				            </xsl:when>
							
							<xsl:when test="type='DQ_CompletenessOmission'">
					        Compleción por omisión
				            </xsl:when>
										
							<xsl:when test="type='DQ_ConceptualConsistency'">
					        Consistencia conceptual
				            </xsl:when>

                            <xsl:when test="type='DQ_TopologicalConsistency'">
					        Consistencia topológica
				            </xsl:when>

                            <xsl:when test="type='DQ_AbsoluteExternalPositionalAccuracy'">
					        Exactitud posicional externa absoluta
				            </xsl:when>

                            <xsl:when test="type='DQ_AccuracyOfATimeMeasurement'">
					        Exactitud de la medida del tiempo
				            </xsl:when>							
						
						    							
						</xsl:choose>
						
						</td>
							
												
						
						
						
						<td id="contenido_tabla" align="center"><xsl:value-of select="./evaluationMethodDescription"/> </td>
						<td id="contenido_tabla" align="center"><xsl:value-of select="./evaluationMethodType"/> </td>
						<td id="contenido_tabla" align="center"><xsl:value-of select="./measureDescription"/> </td>
						<td id="contenido_tabla" align="center"><xsl:value-of select="./nameOfMeasure"/> </td>
						<td id="contenido_tabla" align="center">
							<xsl:value-of select="./result/DQ_QuantitativeResult/value"/>
						 (<xsl:value-of select="./result/DQ_QuantitativeResult/valueUnit"/>)
					</td>
					</tr>
				</xsl:for-each>
			</table>
			<!--  Fin informe -->
			<!-- Descomentar para salto de impresión -->
			<!-- <h1 id="saltoDepagina" style="border-style: none;"></h1> -->
		</fieldset>
	</xsl:template>
	<!-- =============================================== -->
	<!-- ===============  FIN DATOS DE CALIDAD  ============ -->
	<!-- =============================================== -->
	<!-- =============================================== -->
	<!-- ===============  DATOS DE DISTRIBUCION  ============ -->
	<!-- =============================================== -->
	<xsl:template match="distributionInfo/MD_Distribution">
		<A name="distributionInfo/MD_Distribution"/>
		<fieldset>
			<legend>Información de distribución</legend>
			<!--  Formatos de distribución -->
			<br/><span id="info">Formatos de distribución&gt;&gt; </span>
			<xsl:for-each select="distributionFormat">
				<table cellspacing="0">
					<xsl:if test="contains(./MD_Format/name,'Geodatabase') = false and
								  contains(./MD_Format/name,'Repositorio') = false">
							<tr>
								<td id="titulo_tabla" style="width: 15%">Formato:</td>
								<td id="contenido_tabla"><xsl:value-of select="./MD_Format/name"/> </td>
							</tr>
							<tr>
								<td id="titulo_tabla" style="width: 15%">Versión:</td>
								<td id="contenido_tabla"><xsl:value-of select="./MD_Format/version"/> </td>
							</tr>
					</xsl:if>				
					
				</table>
				<br/>
			</xsl:for-each>
					
			<!--  Canales de distribución -->
			<br/><span id="info">Formas de distribución&gt;&gt; </span>
			<xsl:for-each select="transferOptions">
				<table cellspacing="0">
					<xsl:if test="contains(./MD_DigitalTransferOption/unitsOfDistribution,'Geodatabase') = false and
								  contains(./MD_DigitalTransferOption/unitsOfDistribution,'Repositorio') = false">
						<tr>
							<td id="titulo_tabla" style="width:20%">Formato de distribución:</td>
							<td id="contenido_tabla" style="width:80%"><xsl:value-of select="./MD_DigitalTransferOption/unitsOfDistribution"/> </td>
						</tr>
						<xsl:if test="./MD_DigitalTransferOption/transferSize != ''">
							<tr>
								<td id="titulo_tabla" style="width:20%">Tamaño de transferencia (MB):</td>
								<td id="contenido_tabla" style="width:80%"><xsl:value-of select="./MD_DigitalTransferOption/transferSize"/> </td>
							</tr>							
						</xsl:if>
						<xsl:if test="./MD_DigitalTransferOption/onLine != ''">
							<tr>
								<td id="titulo_tabla" colspan="2">Recurso en línea:
						<br/><br/>
									<table cellspacing="0" id="tabla_interna">
										<tr>
											<td id="tabla_interna_titulo" align="left" style="width: 10%">Enlace:</td>
											<td id="tabla_interna_contenido" align="left">
											
													<xsl:if test="string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, '\') ) > 0">	
												<xsl:value-of select="concat(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 1, 39 )
																		, substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 40, 40), '\') )"/>

												<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 40">
												
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 39, 40), '\')) > 0">
														<wbr/><xsl:value-of select="concat('\',concat(substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 39, 40), '\'),
																			  substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 80, 40), '\')))"/>
												</xsl:if>
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 39, 40) , '\')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 39, 1) != '\'">
															<wbr/><xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 40, 40)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 39, 1) = '\'">
															<wbr/><xsl:value-of select="concat('\',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 40, 40), '\'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
											
											
											<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 80">
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 79, 40), '\')) > 0">
														<wbr/><xsl:value-of select="concat(substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 80, 40), '\'),'\')"/>
													<wbr/><xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 80, 40), '\')"/>
												</xsl:if>
												
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 40) , '\')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 1) != '\'">
															<wbr/><xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 80, 40)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 1) = '\'">
															<wbr/><xsl:value-of select="concat('\',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 40), '\'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
											<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 120">
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 119, 40), '\')) > 0">
														<wbr/><xsl:value-of select="concat(substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 120, 40), '\'),'\')"/>
													<wbr/><xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 120, 40), '\')"/>
												</xsl:if>
												
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 119, 40) , '\')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 119, 1) != '\'">
															<wbr/><xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 120, 40)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 119, 1) = '\'">
															<wbr/><xsl:value-of select="concat('\',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 120, 40), '\'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
											<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 160">
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 159, 40), '\')) > 0">
														<wbr/><xsl:value-of select="concat(substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 160, 40), '\'),'\')"/>
													<wbr/><xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 160, 80), '\')"/>
												</xsl:if>
												
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 159, 40) , '\')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 159, 1) != '\'">
															<wbr/><xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 160, 80)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 159, 1) = '\'">
															<wbr/><xsl:value-of select="concat('\',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 160, 80), '\'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
										</xsl:if>
										
										
										<xsl:if test="string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, '/') ) > 0">
											<xsl:value-of select="concat(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 1, 39 )
																		, substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 40, 40), '/'))"/>
												<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 40">
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 39, 40), '/')) > 0">
														<wbr/><xsl:value-of select="concat('/',concat(substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 40, 40), '/'),
																			  substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 80, 40), '/')))"/>
												</xsl:if>
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 39, 40) , '/')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 39, 1) != '/'">
														<xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 40, 40)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 39, 1) = '/'">
														<wbr/><xsl:value-of select="concat('/',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 40, 40), '/'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 80">
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 79, 40), '/')) > 0">
													<wbr/><xsl:value-of select="concat('/',substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 79, 40), '/'))"/>
												</xsl:if>
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 40) , '/')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 1) != '/'">
														<xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 80, 40)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 79, 1) = '/'">
														<wbr/><xsl:value-of select="concat('/',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 80, 40), '/'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
											<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 120">
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 119, 40), '/')) > 0">
														<wbr/><xsl:value-of select="concat(substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 120, 40), '/'),'/')"/>
													<wbr/><xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 120, 40), '/')"/>
												</xsl:if>
												
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 119, 40) , '/')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 119, 1) != '/'">
															<wbr/><xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 120, 40)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 119, 1) = '/'">
															<wbr/><xsl:value-of select="concat('/',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 120, 40), '/'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
											<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage) > 160">
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 159, 40), '/')) > 0">
														<wbr/><xsl:value-of select="concat(substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, 160, 40), '/'),'/')"/>
													<wbr/><xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 160, 80), '/')"/>
												</xsl:if>
												
												<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 159, 40) , '/')) = 0">
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 159, 1) != '/'">
															<wbr/><xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 160, 80)"/>
													</xsl:if>	
													<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 159, 1) = '/'">
															<wbr/><xsl:value-of select="concat('/',substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage , 160, 80), '/'))"/>
													</xsl:if>	
												</xsl:if>
											</xsl:if>
											
										</xsl:if>
										
											<xsl:if test="string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, '/') ) = 0 and
															string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage, '\') ) = 0">
															<xsl:value-of select="./MD_DigitalTransferOption/onLine/CI_OnlineResource/linkage"/>	
											</xsl:if>
											&#160;
												
											</td>
										</tr>
										
										<tr>
											<td id="tabla_interna_titulo" align="left">Nombre:</td>
											<td id="tabla_interna_contenido" align="left">
											
												<xsl:if test="string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, '\') ) > 0">	
												<xsl:value-of select="concat(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 0, 40 )
																			, substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 40, 40), '\') )"/>
	
												
												<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name) > 40">
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 39, 40), '\')) > 0">
													
													\<xsl:value-of select="concat(substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 40, 40), '\'),
																				  substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 80, 40), '\'))"/>
													</xsl:if>
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 39, 40) , '\')) = 0">
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 39, 1) != '\'">
															<xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 40, 40)"/>
														</xsl:if>	
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 39, 1) = '\'">
															\<xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 40, 40), '\')"/>
														</xsl:if>	
													</xsl:if>
												</xsl:if>
												<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name) > 80">
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 79, 40), '\')) > 0">
													
													\<xsl:value-of select="substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 79, 200), '\')"/>
													</xsl:if>
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 79, 40) , '\')) = 0">
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 79, 1) != '\'">
															<xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 80, 200)"/>
														</xsl:if>	
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 79, 1) = '\'">
															\<xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 80, 200), '\')"/>
														</xsl:if>	
													</xsl:if>
												</xsl:if>
											</xsl:if>
											
											
											<xsl:if test="string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, '/') ) > 0">	
												<xsl:value-of select="concat(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 0, 40 )
																			, substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 40, 40), '/') )"/>
	
												
												<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name) > 40">
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 39, 40), '/')) > 0">
													
													/<xsl:value-of select="concat(substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 40, 40), '/'),
																				  substring-before(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 80, 40), '/'))"/>
													</xsl:if>
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 39, 40) , '/')) = 0">
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 39, 1) != '/'">
															<xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 40, 40)"/>
														</xsl:if>	
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 39, 1) = '/'">
															\<xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 40, 40), '/')"/>
														</xsl:if>	
													</xsl:if>
												</xsl:if>
												<xsl:if test="string-length(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name) > 80">
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 79, 40), '/')) > 0">
													
													/<xsl:value-of select="substring-after(	substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, 79, 200), '/')"/>
													</xsl:if>
													
													<xsl:if test="string-length(substring-before(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 79, 40) , '/')) = 0">
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 79, 1) != '/'">
															<xsl:value-of select="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 80, 200)"/>
														</xsl:if>	
														<xsl:if test="substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 79, 1) = '/'">
															\<xsl:value-of select="substring-after(substring(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name , 80, 200), '/')"/>
														</xsl:if>	
													</xsl:if>
												</xsl:if>
											</xsl:if>
											<xsl:if test="string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, '/') ) = 0 and
														string-length(substring-after(./MD_DigitalTransferOption/onLine/CI_OnlineResource/name, '\') ) = 0">
												<xsl:value-of select="./MD_DigitalTransferOption/onLine/CI_OnlineResource/name"/>	
											</xsl:if>
											&#160;
											
											</td>
										</tr>
										<tr>
											<td id="tabla_interna_titulo" align="left">Descripción:</td>
											<td id="tabla_interna_contenido" align="left"><xsl:value-of select="./MD_DigitalTransferOption/onLine/CI_OnlineResource/desc"/> </td>
										</tr>
									</table>
									<br/></td>
							</tr>
						</xsl:if>
					</xsl:if>
				</table>
				<br/>
			</xsl:for-each>
			<!-- Descomentar para salto de impresión -->
			<!-- <h1 id="saltoDepagina" style="border-style: none;"></h1> -->
		</fieldset>
	</xsl:template>
	<!-- =============================================== -->
	<!-- ===============  FIN DATOS DE DISTRIBUCION  ============ -->
	<!-- =============================================== -->
</xsl:stylesheet>
