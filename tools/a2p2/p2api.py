from __future__ import print_function
import json, requests, sys, os, pprint

"""
This file provides an example binding to ESO's RESTful phase 2 programmatic interface.
The data required and returned by many of the API calls is instrument-specific.
In order to fully understand the data and behaviour of the API please consult
the documentation at https://www.eso.org/copdemo/apidoc/
The URL used in each API below should help to locate the corresponding API documentation.

Unless otherwise mentioned, each API call consistently returns a tuple (data, version):
    data    - returned data in JSON format
    version - version of the data, required for future modifications
"""

# $Id: p2api.py 200239 2017-04-02 19:52:33Z tbierwir $
version = '0.3'

API_URL = 'https://www.eso.org/cop/api/v1'
LOGIN_URL = 'https://www.eso.org/cop/api/login'
API_URL_DEMO = 'https://www.eso.org/copdemo/api/v1'
LOGIN_URL_DEMO = 'https://www.eso.org/copdemo/api/login'


class ApiConnection:
    # ---------- public API ----------
    def __init__(self, environment, username, password, debug=False):
        """
        Set environment, log in to the API and obtain access token for further
        calls. Returns authenticated api connection required to invoke other
        API calls.
        usage:
            api = p2api.ApiConnection('production', 52052', 'tutorial')
        """
        self.debug = debug
        self.request_count = 0
        if environment == 'production':
            self.apiUrl = API_URL
            self.loginUrl = LOGIN_URL
        elif environment == 'demo':
            self.apiUrl = API_URL_DEMO
            self.loginUrl = LOGIN_URL_DEMO
        r = requests.post(
            self.loginUrl,
            data={'username': username, 'password': password})
        if r.status_code == requests.codes.ok:
            json = r.json()
            self.access_token = json['access_token']
            print('User', json['username'], 'logged in')
        else:
            print('login failed')
            sys.exit()

    def getRuns(self):
        """
        Retrieve observing runs owned by or delegated to logged-in user.
        usage:
            runs, _ = api.getRuns()
        """
        return self.get('/obsRuns')

    def getRun(self, runId):
        """
        Get a single observing run.
        usage:
            run, _ = api.getRun(runId)
        """
        return self.get('/obsRuns/%d' % runId)

    def getItems(self, containerId):
        """
        Get a list of items in container.
        usage:
            itemList, _ = api.getItems(containerId)
        """
        return self.get('/containers/%d/items' % containerId)
    
    def getFolders(self, containerId):
        """
        Get a list of *Folder* items in container.
        usage:
            folderList = api.getFolders(containerId)
        """
        result=[]
        itemList, _ = self.getItems(containerId)
        for i in range(len(itemList)):
            if itemList[i]['itemType'] == 'Folder':
                result.append(itemList[i])
        return result
    
    def submitRun(self, runId):
        """
        Get a single observing run.
        usage:
            api.submitRun(runId)
        """
        return self.post('/obsRuns/%d/submit' % runId)

    def getPhase1Targets(self, runId):
        """
        Retrieve the list of phase 1 targets.
        usage:
            targets, _ = api.getPhase1Targets(runId)
        """
        return self.get('/obsRuns/%d/phase1/targets' % runId)

    def createItem(self, type, containerId, name):
        """
        Create a new OB, CB, Folder, Group, Concatenation or TimeLink at the end of
        the given container.

        The initial values for the observing constraints of a new OB are those
        specified during phase 1, or the instrument defaults.

        Visitor Mode: Groups, Concatenations and TimeLinks cannot be created.
        Service Mode: Groups, Concatenations and TimeLinks cannot contain other
        containers, only OBs and CBs.

        See also wrappers below to create specific items by name.

        usage:
            ob, obVersion = api.createItem('OB', containerId, 'OliOB')
        """
        return self.post('/containers/%d/items' % containerId, {'itemType': type, 'name': name})
      
    def createFolder(self,containerId,name):
        """
        usage:
            fld = api.createFolder(containerId, 'My New Folder')
        """
        return self.createItem('Folder',containerId,name)

    def createOB(self, containerId, name):
        """
        usage:
            ob, obVersion = api.createOB(containerId, 'OliOB')
        """
        return self.createItem('OB', containerId, name)

    def createCB(self, containerId, name):
        """
        usage:
            cb, cbVersion = api.createCB(containerId, 'OliCB')
        """
        return self.createItem('CB', containerId, name)

    def createFolder(self, containerId, name):
        """
        usage:
            fld, fldVersion = api.createFolder(containerId, 'OliFolder')
        """
        return self.createItem('Folder', containerId, name)

    def createGroup(self, containerId, name):
        """
        usage:
            grp, grpVersion = api.createGroup(containerId, 'OliGroup')
        """
        return self.createItem('Group', containerId, name)

    def createConcatenation(self, containerId, name):
        """
        usage:
            con, conVersion = api.createConcatenation(containerId, 'OliConcatenation')
        """
        return self.createItem('Concatenation', containerId, name)

    def createTimeLink(self, containerId, name):
        """
        usage:
            tl, tlVersion = api.createTimeLink(containerId, 'OliTimeLink')
        """
        return self.createItem('TimeLink', containerId, name)

    def moveOB(self, obId, containerId):
        """
        Move existing OB into destination container.
        TODO: remove list from JSON when DFS-12374 is implemented
        usage:
            api.moveOB(obId, containerId)
        """

        return self.post('/containers/%d/items/append' % containerId,
                         {'itemType': 'OB', 'obId': obId })

    def moveCB(self, obId, containerId):
        """
        Move existing CB into destination container.
        TODO: remove list from JSON when DFS-12374 is implemented
        usage:
            api.moveCB(obId, containerId)
        """
        return self.post('/containers/%d/items/append' % containerId,
                         {'itemType': 'CB', 'obId': obId })

    def getOB(self, obId):
        """
        Get existing OB or CB.
        usage:
            ob, obVersion = api.getOB(obId)
        """
        return self.get('/obsBlocks/%d' % obId)

    def saveOB(self, ob, version):
        """
        Update existing OB or CB.
        The format of an OB is instrument-specific and differs for OBs and CBs.
        The instrument defines the applicable observingConstraints.
        A CB does not have a target. The properties itemType, obId, obStatus,
        ipVersion, exposureTime and executionTime cannot be changed.
        The OB must have the latest ipVersion.
        Visitor Mode: The OB can be updated in any status.
        Service Mode: The OB must have status (-) or (P).
        usage:
            ob, obVersion = api.getOB(obId)
            # modify some OB properties ...
            ob['userPriority'] = 42
            ob['target']['name']         = 'M 32 -- Interacting Galaxies'
            ob['target']['ra']           = '00:42:41.825'
            ob['target']['dec']          = '-60:51:54.610'
            ob['constraints']['name']    = 'My hardest constraints ever'
            ob['constraints']['airmass'] = 1.3
            # save changes
            ob, obVersion = api.saveOB(ob, obVersion)
        """
        return self.put('/obsBlocks/%d' % ob['obId'], ob, version)

    def deleteOB(self, obId, version):
        """
        Delete existing OB or CB.
        Visitor Mode: The OB must have never been executed.
        Service Mode: The OB must have status (-), (P) or (D)
                      and must have never been executed.
        usage:
            ob, obVersion = api.getOB(obId)
            api.deleteOB(ob, obVersion)
        """
        return self.delete('/obsBlocks/%d' % obId, etag=version)

    def verifyOB(self, obId, submit, version):
        """
        Verify OB and submit it for observation.
        If submit is false, an OB can be verified in any status. If submit is
        true, a service mode OB must be in status 'P' or '-' and a visitor mode
        OB must be in status 'P', '-', 'C', 'D', 'F', 'K', 'M', 'R' or 'T'.

        If the OB was created with an earlier instrument package, the response
        is 409 with a list of changes that a migration would perform.

        The exposure time of the OB is recalculated.

        If verification is successful and submit is true, the OB status is
        changed to '+' for a visitor mode OB or to 'D' for a service mode OB.

        Any change in the OB will invalidate the ETags of the OB or its templates
        and the item must be fetched again before the next change.
        usage:
            ob, obVersion = api.getOB(obId)
            response, _ = api.verifyOB(obId, True, obVersion)
            if response['observable']:
                print('Your OB' , obId, ob['name'], 'is observable!')
            else:
                print('OB', obId, 'is >>not observable<<. See messages below.')
                print(' ', '\n  '.join(response['messages']))
        """
        return self.post('/obsBlocks/%d/verify' % obId, {'submit': submit}, version)

    def reviseOB(self, obId, version):
        """
        Back to the drawing board.

        Change the OB status back to 'P' or '-', i.e. mark it as not
        observable. This is the counterpart to verification with submission.

        A visitor mode OB can be in any status. The status is changed to 'P'.
        The OB will be removed from any visitor execution sequences because
        it is no longer in an observable status.

        A service mode OB can only be revised in status 'D'.
        The status is changed to '-'.

        This call invalidates the version of the OB.
        usage:
            ob, obVersion = api.getOB(obId)
            _, obVersion = api.reviseOB(obId, obVersion)
        """
        return self.post('/obsBlocks/%d/revise' % obId, etag=version)

    def duplicateOB(self, obId, containerId=0):
        """
        Duplicate OB.

        Create a new OB or CB identical to this one in the same or a different
        container. The new OB is appended at the end of the container. The
        destination may be in a different observing run.

        The name of the new OB or CB is altered as follows:

        if the name of the OB does not end with underscore '_' followed by a
        sequence of digits, an underscore will be appended followed by a new
        sequence number otherwise, the sequence number will be replaced.

        The new sequence number will start at 2 if there is no other OB with
        the same base name and having a sequence number. If there is such an OB,
        the new sequence number will be the highest sequence number of all such
        OBs + 1. If the name of the OB is too long to have a sequence number
        appended, it will be shortened to accomodate a 5-digit sequence number.

        The new OB or CB will have a new obId, status 'P' and its exposure time
        and execution time will be set to zero. Its migrate flag will be
        recalculated with respect to the destination run. All other fields will
        be the same as in the original OB.

        The new OB will have also
            - identical absolute and sidereal time constraints
            - a copy of all templates attached (in order), with identical
              template parameters
            - the same finding charts attached (in order)
            - and the same ephemeris file attached

        If no containerId is specified, the OB will be duplicated in its current
        container.
        usage:
            duplicatedOB, duplicatedOBVersion = api.duplicateOB(obId)
            duplicatedOB, duplicatedOBVersion = api.duplicateOB(obId, containerId)
        """
        data = {}
        if containerId > 0:
            data = {'containerId': containerId}
        return self.post('/obsBlocks/%d/duplicate' % obId, data)

    def getAbsoluteTimeConstraints(self, obId):
        """
        Get list of absolute time constraints.
        The list is sorted in ascending order by from.
        Date and time are in ISO-8601 format for the UTC timezone.
        usage:
            absTCs, atcVersion = api.getAbsoluteTimeConstraints(obId)
        """
        return self.get('/obsBlocks/%d/timeConstraints/absolute' % obId)

    def saveAbsoluteTimeConstraints(self, obId, timeConstraints, version):
        """
        Set list of absolute time constraints.

        Date and time are in ISO-8601 format and must use UTC timezone.
        Time intervals must not overlap and from must be earlier than to.

        Visitor Mode: The OB can have any status.
        Service Mode: The OB must have status (-), (P), (D), (+), (A), or (M).
        usage:
            absTCs, atcVersion = api.getAbsoluteTimeConstraints(obId)
            api.saveAbsoluteTimeConstraints(obId,[
                {
                    'from': '2017-09-01T00:00',
                    'to': '2017-09-30T23:59'
                },
                {
                    'from': '2017-11-01T00:00',
                    'to': '2017-11-30T23:59'
                }
            ], atcVersion)
        """
        return self.put('/obsBlocks/%d/timeConstraints/absolute' % obId, timeConstraints, version)

    def getSiderealTimeConstraints(self, obId):
        """
        Get list of sidereal time constraints.
        The list is sorted in ascending order by from.
        usage:
            sidTCs, stcVersion = api.getSiderealTimeConstraints(obId)
        """
        return self.get('/obsBlocks/%d/timeConstraints/sidereal' % obId)

    def saveSiderealTimeConstraints(self, obId, timeConstraints, version):
        """
        Set list of sidereal time constraints.

        Time intervals must not overlap. A time interval ending at the
        starting time of another is considered an overlap; combine the
        intervals into a single one.

        Visitor Mode: The OB can have any status.
        Service Mode: The OB must have status (-) or (P).
        usage:
            sidTCs, stcVersion = api.getSiderealTimeConstraints(obId)
            api.saveSiderealTimeConstraints(obId,[
                {
                    'from': '00:00',
                    'to': '01:00'
                },
                {
                    'from': '03:00',
                    'to': '05:00'
                }
            ], stcVersion)
        """
        return self.put('/obsBlocks/%d/timeConstraints/sidereal' % obId, timeConstraints, version)

    def createTemplate(self, obId, name):
        """
        Attach a new template.
        An OB can have only one acquisition template. Attempting to add a
        second one will result in an error.

        The OB must have the latest ipVersion.

        Visitor Mode: The OB can be changed in any status.
        Service Mode: The OB must have status (-) or (P).
        usage:
            tpl, tplVersion = api.createTemplate(obId, 'UVES_blue_acq_slit')
        """
        return self.post('/obsBlocks/%d/templates' % obId, {'templateName': name})

    def getTemplates(self, obId):
        """
        Get all attached templates.
        usage:
            templates, templatesVersion = api.getTemplates(obId)
            for t in templates:
                print ('template found:', t['templateName'])
        """
        return self.get('/obsBlocks/%d/templates' % obId)

    def saveTemplates(self, obId, templates, version):
        """
        Change order of attached templates.

        The list must contain the same items as in the previous GET request.
        Only the order can be changed. Templates must be explicitly added
        with a POST request or deleted with a DELETE request. If there is
        an acquisition template, it must be the first in the list.

        The OB must have the latest ipVersion.

        Visitor Mode: The OB can be changed in any status.
        Service Mode: The OB must have status (-) or (P).
        usage:
            tpls, tplsVersion = api.getTemplates(obId)
            # change order of tpls ...
            tpls, tplsVersion = api.saveTemplates(obId, tpls, tplsVersion)
        """
        return self.put('/obsBlocks/%d/templates' % obId, templates, version)

    def getTemplate(self, obId, templateId):
        """
        Get existing template.

        A parameter value may be null if there is no default value and a
        value was never set.

        For parameters of type 'paramfile' or 'file' a string is returned.
        For a 'paramfile' this is the value of PAF.NAME in the PAF header;
        for a 'file' the first few characters are returned.
        File parameters are set with a separate call.
        usage:
            tpl, tplVersion = api.getTemplate(obId, templateId)
        """
        return self.get('/obsBlocks/%d/templates/%d' % (obId, templateId))

    def setTemplateParams(self, obId, template, params, version):
        """
            Helper call to make it easier to update template parameters.
            Uses saveTemplate() under the hood.
        usage:
            tpl, tplVersion = api.createTemplate(obId, 'UVES_blue_acq_slit')
            tpl, tplVersion  = api.setTemplateParams(obId, tpl, {
                'TEL.GS1.ALPHA': '11:22:33.000',
                'INS.COLL.NAID': 'COLL_SR+7',
                'INS.DPOL.MODE': 'ON'
            }, tplVersion)
        """
        for p in template['parameters']:
            p['value'] = params.get(p['name'], p['value'])
        return self.saveTemplate(obId, template, version)

    def saveTemplate(self, obId, template, version):
        """
        Update existing template.

        The templateId, templateName and type cannot be changed.

        When GET returns a value of null, it can be changed to a proper value
        (or left at null). But a proper value cannot be unset to null.

        File parameters must be set with a separate call; the string value
        returned from GET cannot be changed.

        The OB must have the latest ipVersion.

        Visitor Mode: The OB can be changed in any status.
        Service Mode: The OB must have status (-) or (P).
        """
        return self.put('/obsBlocks/%d/templates/%d' % (obId, template['templateId']), template, etag=version)

    def deleteTemplate(self, obId, templateId, version):
        """
        Delete existing template. [DFS-10901]

        The OB must have the latest ipVersion.

        Visitor Mode: The OB can be changed in any status.
        Service Mode: The OB must have status (-) or (P).
        usage:
            _, tplVersion = api.getTemplate(obId, templateId)
            api.deleteTemplate(obId, templateId, tplVersion)
        """
        return self.delete('/obsBlocks/%d/templates/%d' % (obId, templateId), etag=version)

    def getEphemerisFile(self, obId, filename):
        """
        Get the ephemeris file of this OB and save it into the given filename.

        If the ephemeris file was never saved, or was deleted, an empty file
        is returned.
        usage:
            _, ephVersion = api.getEphemerisFile(obId, 'ephem.txt')
        """
        url = self.apiUrl + '/obsBlocks/%d/ephemeris' % obId
        return self.downloadTextFile(url, filename)

    def saveEphemerisFile(self, obId, filename, version):
        """
        Save the ephemeris file of this OB.
        usage:
            _, ephVersion = api.getEphemerisFile(obId, 'delete_me.txt')
            _, ephVersion = api.saveEphemerisFile(obId, 'ephem.txt', ephVersion)
        """
        return self.uploadFile('PUT', '/obsBlocks/%d/ephemeris' % obId, filename,
                               'text/plain', version)

    def deleteEphemerisFile(self, obId, version):
        """
        Delete the ephemeris file of this OB.

        Service Mode: The OB must have status (-) or (P).
        usage:
            _, ephVersion = api.getEphemerisFile(obId, 'delete_me.txt')
            api.deleteEphemerisFile(obId, ephVersion)
        """
        return self.request('DELETE', '/obsBlocks/%d/ephemeris' % obId, etag=version)

    def getFindingChartNames(self, obId):
        """
        Returns the list of names of the attached finding charts in the order
        of their index. If no finding charts are attached, an empty list will
        be returned.
        usage:
            fcNames, _ = api.getFindingChartNames(obId)
        """
        return self.get('/obsBlocks/%d/findingCharts' % obId)

    def addFindingChart(self, obId, filename):
        """
        Attach a new finding chart in format image/jpeg.
        Up to 5 finding charts can be attached to a single OB.
        The maximum file size is 1MB (2^20 bytes).

        The OB must have the latest ipVersion.

        Visitor Mode: The OB must have status (P), (+), (A)borted or e(X)ecuted.
        Service Mode: The OB must have status (-) or (P).
        usage:
            api.addFindingChart(obId, filename)
        """
        return self.uploadFile('POST', '/obsBlocks/%d/findingCharts' % obId, filename,
                               'image/jpeg')

    def getFindingChart(self, obId, index, filename):
        """
        Download finding chart.
        usage:
            api.getFindingChart(obId, 1, 'fc1.jpg')
        """
        url = self.apiUrl + '/obsBlocks/%d/findingCharts/%d' % (obId, index)
        return self.downloadBinaryFile(url, filename)

    def deleteFindingChart(self, obId, index):
        """
        Delete finding chart.

        The finding chart at the given index is deleted.
        Finding charts with a higher index 'slide down',
        i.e. finding charts are always numbered consecutively
        starting with 1.

        The OB must have the latest ipVersion.

        Visitor Mode: The OB must have status (P), (+), (A)borted or e(X)ecuted.
        Service Mode: The OB must have status (-) or (P).
        usage:
            api.deleteFindingChart(obId, 1)
        """
        return self.delete('/obsBlocks/%d/findingCharts/%d' % (obId, index))

    def getExecutionSequence(self, instrument):
        """
        Retrieve the personal execution sequence for an instrument.
        usage:
            executionSequence, esVersion = api.getExecutionSequence('UVES')
        """
        return self.get('/executionSequences/%s' % instrument)

    def saveExecutionSequence(self, instrument, executionSequence, version):
        """
        Change personal visitor execution sequence for an instrument.

        The list should contain objects with an obId in the desired execution
        order and may also contain OBs from delegated observing runs. Only
        visitor mode OBs in status '+', 'A', 'X' or 'S' can be included.

        The obId is the only required field in each list member, but all fields
        sent by a GET request can be included unchanged.
        usage:
            executionSequence, esVersion = api.getExecutionSequence('UVES')
            executionSequence, esVersion = api.saveExecutionSequence('UVES',
                [{ 'obId': ob1 }, { 'obId': ob2 }], esVersion)
            print('OBs in UVES execution sequence', ', '.join(str(e['obId']) for e in executionSequence))
        """
        return self.put('/executionSequences/%s' % instrument, executionSequence, version)

    def getReadmeSchema(self, runId):
        """
        Retrieve the schema definition for the ReadMe file of this observing run.
        usage:
            readmeSchema, _ = api.getReadmeSchema(runId)
        """
        return self.get('/obsRuns/%d/readme/schema' % runId)

    def getReadme(self, runId):
        """
        Retrieve the ReadMe file.

        Return the ReadMe file of this observing run. If the ReadMe file was
        never saved before, a template with default values (possibly empty
        values) is returned.

        If the instrument package was updated since the ReadMe file was saved,
        the data returned will reflect the changes made to the instrument
        package. This may include new fields with (possibly empty) default
        values which may need to be changed by the application.
        usage:
            readme, readmeVersion = api.getReadme(runId)
        """
        return self.get('/obsRuns/%d/readme' % runId)

    def saveReadme(self, runId, readme, version):
        """
        Update the ReadMe file.
        usage:
            readme, readmeVersion = api.saveReadme(runId, readme, version)
        """
        return self.put('/obsRuns/%d/readme' % runId, readme, version)

    def getInstrumentConstraints(self, instrument, ipVersion):
        """
        Get all observing constraints that OBs using this instrument package
        must define.

        The information returned is useful for building a user interface and
        also contains allowed values or ranges.
        usage:
            insConstraints, _ = api.getInstrumentConstraints('UVES', '98.09')
        """
        return self.get('/instrumentPackages/%s/%s/instrumentConstraints' % (instrument, ipVersion))

    def getTemplateSignatures(self, instrument, ipVersion):
        """
        List all template signatures that can be attached to OBs using this
        instrument package.
        usage:
            tplSignatures, _ = api.getTemplateSignatures('UVES', '98.09')
        """
        return self.get('/instrumentPackages/%s/%s/templateSignatures' % (instrument, ipVersion))

    def getTemplateSignature(self, instrument, ipVersion, templateName):
        """
        Get a specific template signature.
        usage:
            tplSignature, _ = api.getTemplateSignature('UVES', '98.08', 'UVES_blue_acq_slit')
        """
        return self.get('/instrumentPackages/%s/%s/templateSignatures/%s' % (instrument, ipVersion, templateName))

    def getPrettyPrinter(self):
        """ return a pretty printer object that is configured to remove the unicode prefix """
        pp = pprint.PrettyPrinter(indent=4)
        pp.format = no_unicode
        return pp

    # ---------- private methods ----------
    def request(self, method, url, data=None, etag=None):
        self.request_count += 1

        # configure request headers
        assert self.access_token is not None
        headers = {
            'Authorization': 'Bearer ' + self.access_token,
            'Accept': 'application/json'
        }
        if data is not None:
            headers['Content-Type'] = 'application/json'
        if etag is not None:
            headers['If-Match'] = etag

        # make request
        url = self.apiUrl + url
        if self.debug:
            print(method, url, data)
        r = requests.request(method, url, headers=headers, data=json.dumps(data))
        content_type = r.headers['Content-Type'].split(';')[0]
        etag = r.headers.get('ETag', None)

        # handle response
        if 200 <= r.status_code < 300:
            if content_type == 'application/json':
                data = r.json()
                return data, etag
            else:
                return None, etag
        else:
            if content_type == 'application/json' and 'error' in r.json():
                print('Error', r.status_code, method, url, ':', r.json()['error'])
                sys.exit()
            else:
                print('oops unknown error', r.status_code)
                sys.exit()

    def uploadFile(self, method, url, filename, contentType, etag=None):
        basename = os.path.basename(filename)
        assert self.access_token is not None
        headers = {
            'Authorization': 'Bearer ' + self.access_token,
            'Content-Disposition': 'inline; filename="%s"' % basename,
            'Content-Type': contentType
        }
        if etag is not None:
            headers['If-Match'] = etag
        with open(filename, 'rb') as f:
            r = requests.request(method, self.apiUrl + url, data=f, headers=headers)
            if r.status_code == 201 or r.status_code == 204:
                etag = r.headers.get('ETag', None)
                return basename, etag
            else:
                content_type = r.headers['Content-Type'].split(';')[0]
                if content_type == 'application/json' and 'error' in r.json():
                    print('Error', r.status_code, method, url, ':', r.json()['error'])
                    sys.exit()
                else:
                    print('oops unknown error', r.status_code)
                    sys.exit()

    def downloadTextFile(self, url, filename):
        assert self.access_token is not None
        headers = {
            'Authorization': 'Bearer ' + self.access_token,
            'Accept': 'text/plain'
        }
        r = requests.request('GET', url, stream=True, headers=headers)
        with open(filename, 'wb') as f:
            for line in r.iter_lines():
                f.write(line + '\n')
        return None, r.headers.get('ETag', None)

    def downloadBinaryFile(self, url, filename):
        assert self.access_token is not None
        headers = {
            'Authorization': 'Bearer ' + self.access_token,
            'Accept': 'text/plain'
        }
        r = requests.request('GET', url, stream=True, headers=headers)
        with open(filename, 'wb') as f:
            for chunk in r.iter_content(chunk_size=128):
                f.write(chunk)
        return None, None

    def get(self, url):
        return self.request('GET', url)

    def put(self, url, data, etag=None):
        return self.request('PUT', url, data, etag)

    def post(self, url, data=None, etag=None):
        return self.request('POST', url, data, etag)

    def delete(self, url, etag=None):
        return self.request('DELETE', url, etag=etag)

# ---- TODO: add bindings for the following APIs
#     getContainer
#     saveContainer
#     deleteContainer
#     getItems
#     saveItems
#     moveItem
#     getFileParameterETag
#     getFileParameter
#     saveFileParameter
#     migrateOB
#     getOBsSummary

# ---------- pretty printing support ----------
def no_unicode(object, context, maxlevels, level):
    """ change unicode u'foo' to string 'foo' when pretty printing"""
    if pprint._type(object) is unicode:
        object = str(object)
    return pprint._safe_repr(object, context, maxlevels, level)
