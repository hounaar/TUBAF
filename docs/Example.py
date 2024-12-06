#!/usr/bin/env python3

import sys
import xmlrpc.client
import logging
from time import sleep
"""
This is an example how to use the XML-RPC interface of horstFX
"""
class Client:

    def __init__(self, user="horstFX", password="WZA3P9", url="10.42.0.1:8080/xml-rpc"):
        self.URL = "@" + url
        '''Username and password (only needed for some calls, including RobotMoveCalls)'''
        self.USERNAME = user
        self.PASSWORD = password
        '''Construct a client with the HorstFX server url, username and password '''
        self.client = xmlrpc.client.ServerProxy("http://" + self.USERNAME + ":" + self.PASSWORD + self.URL)
        print("Initialized xmlrpc client for user '" + self.USERNAME + "'")


    """ Example call to get the robot position"""
    def getCurrentRobotPosition(self):
        pos = self.client.HorstFX.Robotcontrol.getCurrentRobotPosition()
        print ("q0: " + str(pos.get('q0')) + " q1: " + str(pos.get("q1")) + " q2: " + str(pos.get("q2")) + " q3: " + str(pos.get("q3")) +
               "\nrx: " + str(pos.get("rx")) + " ry: " + str(pos.get("ry")) + " rz: " + str(pos.get("rz")) +
               "\nx: " + str(pos.get("x")) + " y: " + str(pos.get("y")) + " z: " + str(pos.get("z")))


    """ Example call to get the current robot joint position"""
    def getCurrentRobotJoints(self):
        joints = self.client.HorstFX.Robotcontrol.getCurrentRobotJoints()
        print ("j1: " + str(joints.get('j1')) + " j2: " + str(joints.get('j2')) + " j3: " + str(joints.get('j3')) +
               " j4: " + str(joints.get('j4')) + " j5: " + str(joints.get('j5')) + " j6: " + str(joints.get('j6')))


    """ Example call to move the robot (joint movement)"""
    def moveJoint(self, x, y, z, q0, q1, q2, q3, speed):
        result = self.client.HorstFX.v1.Robotcontrol.moveJoint(x, y, z, q0, q1, q2, q3, speed) #speed
        return result


    """ Example call to move the robot (linear movement)"""
    def moveLinear(self, x, y, z, q0, q1, q2, q3, speed):
        result = self.client.HorstFX.v1.Robotcontrol.moveLinear(x, y, z, q0, q1, q2, q3, speed) #speed
        return result

    def move(self, move_array=[]):

        result = self.client.HorstFX.v2.Robotcontrol.moveAdvanced(move_array)
        return result

    def moveTrajectory(self, trajectory_joints=[], error_on_constraints_violation=True):

        result = self.client.HorstFX.v5.Robotcontrol.moveTrajectory(trajectory_joints, error_on_constraints_violation)
        return result

    """ Example call to get Inputs"""
    def getInput(self, input):
        result = self.client.HorstFX.v1.Robotcontrol.getInput(input)
        return result


    """ Example call to set Outputs"""
    def setOutput(self, output_name, value):
        result = self.client.HorstFX.v1.Robotcontrol.setOutput(output_name, value)
        return result

    """ Example call to set the tool"""
    def setTool(self, tool_name):
        result = self.client.HorstFX.v3.Robotcontrol.setTool(tool_name)
        return result

    """ Example call to set the variable nextPose"""
    def nextPose(self, x, y, z, q0, q1, q2, q3):
        result = self.client.HorstFX.v3.Variable.nextPose(x, y, z, q0, q1, q2, q3)
        return result

    """ Example call to get the nextPose """
    def getNextPose(self):
        result = self.client.HorstFX.v3.Variable.getNextPose()
        return result

    """ Example call to set the variable nextJoints"""
    def nextJoints(self, joint1, joint2, joint3, joint4, joint5, joint6):
        result = self.client.HorstFX.v3.Variable.nextJoints(joint1, joint2, joint3, joint4, joint5, joint6)
        return result

    """ Example call to get the variable nextJoints"""
    def getNextJoints(self):
        result = self.client.HorstFX.v3.Variable.getNextJoints()
        return result

    """ Example call to set the register"""
    def setRegister(self, registerIndex, value):
        result = self.client.HorstFX.v3.Variable.setRegister(registerIndex, float(value))
        return result

    """ Example call to get the given register value"""
    def getRegister(self, registerIndex):
        result = self.client.HorstFX.v3.Variable.getRegister(registerIndex)
        return result

    """ Example call to execute a program"""
    def execute(self, script):
        result = self.client.HorstFX.v3.Program.execute(script)
        return result

    """ Example call to pause a program"""
    def pause(self):
        result = self.client.HorstFX.v3.Program.pause()
        return result

    """ Example call to find out if a program is running"""
    def isRunning(self):
        result = self.client.HorstFX.v3.Program.isRunning()
        return result

    """ Example call to abort a program"""
    def abort(self):
        result = self.client.HorstFX.v3.Program.abort()
        return result

    """ Example call to proceed a program"""
    def proceed(self):
        result = self.client.HorstFX.v3.Program.proceed()
        return result

    """ Example call to proceed a program"""
    def play(self):
        result = self.client.HorstFX.v6.Program.play()
        return result

    """ Example call to proceed a program"""
    def getGlobalSpeed(self):
        result = self.client.HorstFX.v5.Program.getGlobalSpeed()
        return result

    """ Example call to proceed a program"""
    def setGlobalSpeed(self, value):
        result = self.client.HorstFX.v5.Program.setGlobalSpeed(value)
        return result

    """ Example call to proceed a program"""
    def safetyStatus(self):
        result = self.client.HorstFX.v4.Safety.status()
        return result

    """ Example call to proceed a program"""
    def confirmEmergencyStop(self):
        result = self.client.HorstFX.v4.Safety.confirmEmergencyStop()
        return result

    """ Example call to proceed a program"""
    def confirmInternalError(self):
        result = self.client.HorstFX.v4.Safety.confirmInternalError()
        return result

    """ Example call to proceed a program"""
    def confirmChangeOperatingMode(self):
        result = self.client.HorstFX.v4.Safety.confirmChangeOperatingMode()
        return result


logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("debug.log"),
        logging.StreamHandler()
    ]
)

Client_object=Client()
print(Client_object)
position=Client_object.getCurrentRobotPosition()